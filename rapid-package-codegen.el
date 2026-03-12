;;; rapid-package-codegen.el --- Code generator for rapid-package -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cottontailia

;; Author: Cottontailia
;; Additional-Author: AI Assistant
;; URL: https://github.com/cottontailia/rapid-package
;; License: CC0

;;; Commentary:
;;
;; Expansion-form generator for rapid-package macros.
;; Takes normalized plists from the DSL parser and produces Lisp forms.
;; Buckets are managed as tail-tracked lists (rapid-package--tl) to avoid
;; the O(N^2) cost of repeated append.

;;; Code:

(require 'cl-lib)
(require 'rapid-package-tl)

;;; Unquote helper

(defun rapid-package--codegen-unquote (val)
  "Unwrap (\\, EXPR) to EXPR for code emission; return other values unchanged.
DSL unquote forms are preserved in the IR so JSON round-trip works correctly.
This function strips the marker at the final codegen step."
  (if (and (consp val) (eq (car val) '\,))
      (cadr val)
    val))

;;; Position helpers

(defun rapid-package--codegen-position-key (position)
  "Return the anchor symbol from POSITION spec.
POSITION can be SYM, (:before SYM), or (:after SYM)."
  (cond
   ((symbolp position) position)
   ((and (listp position) (memq (car position) '(:before :after))) (cadr position))
   (t (error "Invalid position spec: %S" position))))

(defun rapid-package--codegen-position-side (position)
  "Return :before or :after from POSITION.  A bare symbol means :after."
  (if (symbolp position) :after (car position)))

;;; Bucket helpers

(defun rapid-package--codegen-bucket-new (keys)
  "Return a fresh bucket plist with each key in KEYS mapped to an empty TL."
  (let (buckets)
    (dolist (k keys)
      (setq buckets (plist-put buckets k (rapid-package--tl-new))))
    buckets))

(defun rapid-package--codegen-bucket-extend! (buckets key forms)
  "Append FORMS to bucket KEY in BUCKETS.  FORMS ownership passes to the bucket."
  (rapid-package--tl-extend! (plist-get buckets key) forms)
  buckets)

(defun rapid-package--codegen-bucket-append! (buckets key form)
  "Append single FORM to bucket KEY in BUCKETS."
  (rapid-package--tl-append! (plist-get buckets key) form)
  buckets)

(defun rapid-package--codegen-bucket-prepend-extend! (buckets key forms)
  "Prepend FORMS to bucket KEY in BUCKETS (:before semantics)."
  (rapid-package--tl-prepend-extend! (plist-get buckets key) forms)
  buckets)

(defun rapid-package--codegen-flatten-buckets (buckets order)
  "Return a flat list of all forms in BUCKETS in ORDER."
  (cl-mapcan (lambda (key) (rapid-package--tl-value (plist-get buckets key)))
             order))

;;; Expanders

(defun rapid-package--codegen-expanders (expanders buckets plist name for)
  "Run EXPANDERS for FOR, inserting forms into BUCKETS.  Returns BUCKETS."
  (dolist (entry expanders)
    (when (memq (plist-get entry :for) `(both ,for))
      (let* ((kw  (plist-get entry :keyword))
             (val (plist-get plist kw)))
        (when val
          (let* ((pos       (plist-get entry :position))
                 (anchor    (rapid-package--codegen-position-key pos))
                 (side      (rapid-package--codegen-position-side pos))
                 (new-forms (funcall (plist-get entry :expander) val name plist)))
            (unless (plist-member buckets anchor)
              (error "Invalid anchor %S for %S" anchor for))
            (if (eq side :before)
                (rapid-package--codegen-bucket-prepend-extend! buckets anchor new-forms)
              (rapid-package--codegen-bucket-extend! buckets anchor new-forms)))))))
  buckets)

;;; Form generators

(defun rapid-package--codegen-traverse-bindings (groups callback)
  "Call CALLBACK (KEY CMD DOC KEYMAP) for each binding in IR GROUPS."
  (dolist (group groups)
    (let ((maps     (plist-get group :map))
          (bindings (plist-get group :bind)))
      (dolist (map (or maps '(nil)))
        (dolist (binding bindings)
          (funcall callback
                   (plist-get binding :key)
                   (plist-get binding :command)
                   (plist-get binding :description)
                   map))))))

(defun rapid-package--codegen-normalize-key (key)
  "Normalize KEY to a string suitable for `keymap-global-set' / `keymap-set'.
Vectors like [remap undo] are converted via `key-description'.
Strings are returned as-is."
  (if (vectorp key) (key-description key) key))

(defun rapid-package--codegen-normalize-cmd (cmd)
  "Normalize CMD to a plain symbol for use in keymap forms.
Accepts:
  SYMBOL          -> SYMBOL   (used as-is)
  (function SYM)  -> SYM      (#\\='foo unwrapped)
  (quote SYM)     -> SYM      (\\='foo unwrapped)
Signals an error for anything else (e.g. lambdas)."
  (cond
   ((symbolp cmd) cmd)
   ((and (consp cmd)
         (memq (car cmd) '(function quote))
         (symbolp (cadr cmd))
         (null (cddr cmd)))
    (cadr cmd))
   (t (error ":bind command must be a symbol or #'SYMBOL, got: %S" cmd))))

(defun rapid-package--codegen-bind-forms (bindings &optional override)
  "Return keymap-set forms for BINDINGS.
If OVERRIDE is non-nil, global bindings target `override-global-map'.

CMD may be a symbol or #\\='SYMBOL (both produce #\\='CMD in output),
or a string (used as-is, for key-translation-map and similar uses)."
  (when bindings
    (let ((tl (rapid-package--tl-new)))
      (rapid-package--codegen-traverse-bindings
       bindings
       (lambda (key cmd _doc keymap)
         (let ((key      (rapid-package--codegen-normalize-key key))
               (cmd-form (if (stringp cmd)
                             cmd
                           `#',(rapid-package--codegen-normalize-cmd cmd))))
           (rapid-package--tl-append!
            tl
            (if keymap
                `(keymap-set ,(if override 'override-global-map keymap) ,key ,cmd-form)
              (if override
                  `(keymap-set override-global-map ,key ,cmd-form)
                `(keymap-global-set ,key ,cmd-form)))))))
      (rapid-package--tl-value tl))))

(defun rapid-package--codegen-hook-forms (hooks)
  "Return add-hook forms for normalized HOOKS.
Each entry's :function is a plain symbol (guaranteed by the parser),
so the output uses #\\=' for proper compile-time function resolution."
  (when hooks
    (let ((tl (rapid-package--tl-new)))
      (dolist (entry hooks)
        (cond
         ((plist-get entry :modes)
          (dolist (mode (plist-get entry :modes))
            (rapid-package--tl-append!
             tl `(add-hook ',mode #',(plist-get entry :function)))))
         ((plist-get entry :mode)
          (rapid-package--tl-append!
           tl `(add-hook ',(plist-get entry :mode) #',(plist-get entry :function))))
         (t (error "Invalid hook entry: %S" entry))))
      (rapid-package--tl-value tl))))

(defun rapid-package--codegen-custom-forms (customs)
  "Return customize-set-variable forms for normalized CUSTOMS."
  (when customs
    (mapcar (lambda (entry)
              `(customize-set-variable ',(plist-get entry :variable)
                                       ,(rapid-package--codegen-unquote
                                         (plist-get entry :value))))
            customs)))

(defun rapid-package--codegen-custom-face-forms (faces)
  "Return `face-spec-set' forms for normalized FACES."
  (when faces
    (mapcar (lambda (entry)
              (let* ((face  (plist-get entry :variable))
                     (qval  (plist-get entry :value))
                     ;; qval is (quote SPEC) or (\, EXPR); unwrap to get raw SPEC/EXPR
                     (spec  (cond
                             ((and (consp qval) (eq (car qval) 'quote)) (cadr qval))
                             ((and (consp qval) (eq (car qval) '\,))    (cadr qval))
                             (t qval))))
                (if (and (consp qval) (eq (car qval) '\,))
                    `(face-spec-set ',face ,spec)
                  `(face-spec-set ',face ',spec))))
            faces)))

(defun rapid-package--codegen-bind-keymap-forms (keymaps)
  "Return keymap-global-set forms for normalized KEYMAPS."
  (when keymaps
    (mapcar (lambda (entry)
              `(keymap-global-set
                ,(rapid-package--codegen-normalize-key (plist-get entry :variable))
                ',(plist-get entry :value)))
            keymaps)))

(defun rapid-package--codegen-env-forms (pairs)
  "Return setenv forms for normalized env PAIRS.
Each pair has :variable (env var name string) and :value (string or nil).
nil value causes setenv to unset the variable."
  (when pairs
    (mapcar (lambda (entry)
              `(setenv ,(plist-get entry :variable)
                       ,(rapid-package--codegen-unquote (plist-get entry :value))))
            pairs)))

(defun rapid-package--codegen-env-path-forms (entries)
  "Return setenv / exec-path forms for normalized env-path ENTRIES.
Each entry has :var (string), :dir (string), :op (:prepend or :append).
PATH entries also sync `exec-path'."
  (when entries
    (mapcar
     (lambda (e)
       (let* ((var    (plist-get e :var))
              (dir    (rapid-package--codegen-unquote (plist-get e :dir)))
              (op     (plist-get e :op))
              (path-p (equal var "PATH")))
         (pcase op
           (:prepend
            (if path-p
                `(let ((dir ,dir))
                   (setenv "PATH" (concat dir path-separator (getenv "PATH")))
                   (add-to-list 'exec-path dir))
              `(let ((dir ,dir))
                 (setenv ,var (concat dir path-separator (getenv ,var))))))
           (:append
            (if path-p
                `(let ((dir ,dir))
                   (setenv "PATH" (concat (getenv "PATH") path-separator dir))
                   (add-to-list 'exec-path dir t))
              `(let ((dir ,dir))
                 (setenv ,var (concat (getenv ,var) path-separator dir)))))
           (_ (error "rapid-package--codegen-env-path-forms: unknown op %S" op)))))
     entries)))

(defun rapid-package--codegen-variable-forms (pairs &optional use-default)
  "Return setq (or setq-default if USE-DEFAULT) forms for normalized PAIRS."
  (when pairs
    (mapcar (lambda (entry)
              (let ((var (plist-get entry :variable))
                    (val (rapid-package--codegen-unquote (plist-get entry :value))))
                (if use-default `(setq-default ,var ,val) `(setq ,var ,val))))
            pairs)))

(defun rapid-package--codegen-unbind-forms (groups)
  "Return keymap-unset / keymap-global-unset forms for IR GROUPS."
  (when groups
    (let ((tl (rapid-package--tl-new)))
      (dolist (group groups)
        (let ((maps (plist-get group :map))
              (keys (plist-get group :unbind)))
          (dolist (map (or maps '(nil)))
            (dolist (key keys)
              (let ((key (rapid-package--codegen-normalize-key key)))
                (rapid-package--tl-append!
                 tl (if map
                        `(keymap-unset ,map ,key)
                      `(keymap-global-unset ,key))))))))
      (rapid-package--tl-value tl))))


;;; :with codegen

(defun rapid-package--codegen-with-target-info (target)
  "Compute kind, hook, and map from TARGET symbol.
Returns a plist: (:kind KIND :hook HOOK :map MAP)
  KIND: :mode, :hook, or :map
  HOOK: the hook symbol (TARGET-hook or TARGET itself if -hook suffix)
  MAP: the map symbol (TARGET-map or TARGET itself if -map suffix)"
  (let ((name (symbol-name target)))
    (cond
     ((string-suffix-p "-hook" name)
      (list :kind :hook
            :hook target
            :map nil))
     ((string-suffix-p "-map" name)
      (list :kind :map
            :hook nil
            :map target))
     (t
      (list :kind :mode
            :hook (intern (concat name "-hook"))
            :map (intern (concat name "-map")))))))

(defun rapid-package--codegen-with-fn-name (prefix name mode-sym id-sym)
  "Return the generated function symbol for a :with block.
PREFIX is the string prefix (\"rapid-package--with\" or
\"rapid-package--with-conf\").
NAME is the package/category symbol.
MODE-SYM is the mode symbol (before normalization).
ID-SYM is the optional explicit ID, or nil.

Trailing -hook and -map suffixes are stripped from MODE-SYM in the name."
  (let* ((s (symbol-name mode-sym))
         (mode-part (cond ((string-suffix-p "-hook" s)
                           (substring s 0 (- (length s) 5)))
                          ((string-suffix-p "-map" s)
                           (substring s 0 (- (length s) 4)))
                          (t s)))
         (base (concat prefix "/" (symbol-name name) "/" mode-part)))
    (intern (if id-sym
                (concat base "/" (symbol-name id-sym))
              base))))

(defun rapid-package--codegen-with-forms (with-blocks name prefix)
  "Return forms for all :with BLOCKS for package/conf NAME.
PREFIX is the function-name prefix string:
  package -> \"rapid-package--with\"
  conf    -> \"rapid-package--with-conf\"

Blocks use simplified IR with :target instead of :kind/:mode/:hook/:map.
Kind, hook, and map are computed from :target at codegen time.

:kind :mode / :hook - generates defun + add-hook:
  (defun FN () (setq-local VAR VAL) ... (FN1) (FN2) ... (keymap-set ...) ...)
  (add-hook \\='HOOK #\\='FN)
  Body order: :local (setq-local) then :hook (fn calls) then :bind (keymap-set).

:kind :map - no defun/add-hook; keymap forms emitted directly:
  (keymap-set MAP KEY #\\='CMD) ...
  (keymap-unset MAP KEY) ..."
  (when with-blocks
    (let ((tl (rapid-package--tl-new)))
      (dolist (block with-blocks)
        (let* ((target    (plist-get block :target))
               (info      (rapid-package--codegen-with-target-info target))
               (kind      (plist-get info :kind))
               (hook      (plist-get info :hook))
               (map       (plist-get info :map))
               (id-sym    (plist-get block :id))
               (locals    (plist-get block :local))
               (hooks     (plist-get block :hook))
               (binds     (plist-get block :bind))
               (unbinds   (plist-get block :unbind)))
          (if (eq kind :map)
              ;; :kind :map - emit keymap forms directly, no defun/add-hook
              (progn
                (dolist (pair binds)
                  (let ((key      (rapid-package--codegen-normalize-key (plist-get pair :key)))
                        (cmd-form (if (stringp (plist-get pair :command))
                                      (plist-get pair :command)
                                    `#',(rapid-package--codegen-normalize-cmd (plist-get pair :command)))))
                    (rapid-package--tl-append! tl `(keymap-set ,map ,key ,cmd-form))))
                (dolist (key unbinds)
                  (rapid-package--tl-append!
                   tl `(keymap-unset ,map ,(rapid-package--codegen-normalize-key key)))))
            ;; :kind :mode / :hook - wrap in defun + add-hook
            (let* ((fn-name (rapid-package--codegen-with-fn-name prefix name target id-sym))
                   (body-tl (rapid-package--tl-new)))
              (dolist (entry locals)
                (rapid-package--tl-append! body-tl
                                           `(setq-local ,(plist-get entry :variable)
                                                        ,(rapid-package--codegen-unquote
                                                          (plist-get entry :value)))))
              (dolist (fn hooks)
                (rapid-package--tl-append! body-tl `(,fn)))
              (dolist (pair binds)
                (let ((key      (rapid-package--codegen-normalize-key (plist-get pair :key)))
                      (cmd-form (if (stringp (plist-get pair :command))
                                    (plist-get pair :command)
                                  `#',(rapid-package--codegen-normalize-cmd (plist-get pair :command)))))
                  (rapid-package--tl-append! body-tl
                                             `(keymap-set ,map ,key ,cmd-form))))
              (dolist (key unbinds)
                (rapid-package--tl-append!
                 body-tl `(keymap-unset ,map ,(rapid-package--codegen-normalize-key key))))
              (let ((body-forms (rapid-package--tl-value body-tl)))
                (rapid-package--tl-append! tl `(defun ,fn-name () ,@body-forms))
                (rapid-package--tl-append! tl `(add-hook ',hook #',fn-name)))))))
      (rapid-package--tl-value tl))))



(defun rapid-package--codegen-get-flag (plist key &optional auto-keys default)
  "Return KEY from PLIST, auto-defaulting to t if any AUTO-KEYS are present."
  (cond
   ((plist-member plist key) (plist-get plist key))
   ((and auto-keys (cl-some (lambda (k) (plist-get plist k)) auto-keys)) t)
   (t default)))

(defun rapid-package--codegen-ensure-p (plist)
  "Return non-nil if PLIST requests package installation."
  (rapid-package--codegen-get-flag plist :ensure))

(defun rapid-package--codegen-defer-p (plist)
  "Return non-nil if PLIST requests deferred loading.
Auto-defers when :hook, :bind, :mode, :magic, :interpreter, or
:commands is present."
  (rapid-package--codegen-get-flag plist :defer
                                   '(:hook :bind :mode :magic :interpreter :commands)))

(defun rapid-package--codegen-demand-p (plist)
  "Return non-nil if PLIST requests immediate loading via :demand."
  (plist-get plist :demand))

;;; Body wrapper

(defun rapid-package--wrap-body (forms)
  "Wrap FORMS in progn if needed; return nil for empty, unwrapped for singleton."
  (cond
   ((null forms) nil)
   ((null (cdr forms)) (car forms))
   (t `(progn ,@forms))))

;;; Package codegen

(defun rapid-package--codegen-package (pkg-name expanders p)
  "Return the expansion form for PKG-NAME given parsed plist P."
  (let* ((ensure-p  (rapid-package--codegen-ensure-p p))
         (demand-p  (rapid-package--codegen-demand-p p))
         (defer-p   (and (rapid-package--codegen-defer-p p) (not demand-p)))
         (after          (plist-get p :after))
         (preface-body   (plist-get p :preface))
         (init-body      (plist-get p :init))
         (require-features (plist-get p :require))
         (config-body    (plist-get p :config))
         (commands       (plist-get p :commands))
         (modes          (plist-get p :mode))
         (modes-enable   (plist-get p :mode-enable))
         (modes-disable  (plist-get p :mode-disable))
         (interpreters   (plist-get p :interpreter))
         (magics         (plist-get p :magic))
         (hooks          (plist-get p :hook))
         (bindings       (plist-get p :bind))
         (bindings*      (plist-get p :bind*))
         (bind-keymaps   (plist-get p :bind-keymap))
         (unbind-keys    (plist-get p :unbind))
         (customs        (plist-get p :custom))
         (custom-faces   (plist-get p :custom-face))
         (variable-pairs (plist-get p :variable))
         (variable-default-pairs (plist-get p :variable-default))
         (diminish-mode  (plist-get p :diminish))
         (delight-spec   (plist-get p :delight))
         (pin-archive    (plist-get p :pin))
         (vc-spec        (plist-get p :vc))
         (load-path-dirs (plist-get p :load-path))
         (with-blocks    (plist-get p :with))
         (env-pairs        (plist-get p :env))
         (env-path-entries (plist-get p :env-path)))

    (let* ((buckets (rapid-package--codegen-bucket-new
                     '(:preface :pin :install :init :trigger
                                :config-pre :config-post)))
           (bucket-order '(:preface :pin :install :init :trigger)))

      ;; :load-path (prepend to preface so load-path is set before ensure/require)
      (dolist (dir load-path-dirs)
        (rapid-package--codegen-bucket-append!
         buckets :preface `(add-to-list 'load-path ,dir)))

      ;; :preface
      (when preface-body
        (rapid-package--codegen-bucket-extend! buckets :preface preface-body))
      (when env-path-entries
        (rapid-package--codegen-bucket-extend!
         buckets :preface (rapid-package--codegen-env-path-forms env-path-entries)))

      ;; :pin
      (when pin-archive
        (rapid-package--codegen-bucket-append!
         buckets :pin
         `(add-to-list 'package-pinned-packages '(,pkg-name . ,pin-archive))))

      ;; :install
      (cond
       (vc-spec
        (rapid-package--codegen-bucket-append!
         buckets :install
         `(unless (package-installed-p ',pkg-name)
            (package-vc-install ',vc-spec))))
       (ensure-p
        (let ((target (if (and (symbolp ensure-p) (not (eq ensure-p t)))
                          ensure-p pkg-name)))
          (rapid-package--codegen-bucket-append!
           buckets :install
           `(unless (package-installed-p ',target)
              (package-install ',target))))))

      ;; :init
      (when env-pairs
        (rapid-package--codegen-bucket-extend!
         buckets :init (rapid-package--codegen-env-forms env-pairs)))
      (when init-body
        (rapid-package--codegen-bucket-append! buckets :init `(progn ,@init-body)))

      ;; :require
      (when require-features
        (dolist (feature require-features)
          (rapid-package--codegen-bucket-append!
           buckets :config-pre `(require ',feature))))

      ;; :trigger - autoloads, :mode, :magic, :interpreter
      (let ((tl (rapid-package--tl-new)))
        (dolist (cmd commands)
          (rapid-package--tl-append! tl `(autoload ',cmd ,(symbol-name pkg-name) nil t)))
        (when bindings
          (rapid-package--codegen-traverse-bindings
           bindings
           (lambda (_key cmd _doc _map)
             (let ((cmd-sym (rapid-package--codegen-normalize-cmd cmd)))
               (rapid-package--tl-append!
                tl `(autoload ',cmd-sym ,(symbol-name pkg-name) nil t))))))
        (dolist (pat modes)
          (rapid-package--tl-append! tl `(add-to-list 'auto-mode-alist '(,pat . ,pkg-name))))
        (dolist (pat magics)
          (let ((entry (if (consp pat)
                           pat
                         (cons pat pkg-name))))
            (rapid-package--tl-append! tl `(add-to-list 'magic-mode-alist ',entry))))
        (dolist (interp interpreters)
          (rapid-package--tl-append!
           tl `(add-to-list 'interpreter-mode-alist '(,interp . ,pkg-name))))
        (rapid-package--tl-concat! (plist-get buckets :trigger) tl))

      ;; :bind - separate global/map forms, place in trigger or config-pre
      (when bindings
        (let ((global-tl (rapid-package--tl-new))
              (map-tl    (rapid-package--tl-new))
              (defer-explicit-p (and (plist-member p :defer) (plist-get p :defer))))
          (rapid-package--codegen-traverse-bindings
           bindings
           (lambda (key cmd _doc keymap)
             (let ((key      (rapid-package--codegen-normalize-key key))
                   (cmd-sym  (rapid-package--codegen-normalize-cmd cmd)))
               (rapid-package--tl-append!
                (if keymap map-tl global-tl)
                (if keymap
                    `(keymap-set ,keymap ,key #',cmd-sym)
                  `(keymap-global-set ,key #',cmd-sym))))))
          (let ((bucket (if (or after defer-explicit-p) :config-pre :trigger)))
            (rapid-package--tl-concat! (plist-get buckets bucket) global-tl)
            (rapid-package--tl-concat! (plist-get buckets bucket) map-tl))))

      ;; :config-pre
      (let ((tl (rapid-package--tl-new)))
        (rapid-package--tl-extend! tl (rapid-package--codegen-variable-forms variable-pairs))
        (rapid-package--tl-extend! tl (rapid-package--codegen-variable-forms variable-default-pairs t))
        (rapid-package--tl-extend! tl (rapid-package--codegen-custom-forms customs))
        (rapid-package--tl-extend! tl (rapid-package--codegen-custom-face-forms custom-faces))
        (rapid-package--tl-extend! tl (rapid-package--codegen-bind-forms bindings* t))
        (rapid-package--tl-extend! tl (rapid-package--codegen-bind-keymap-forms bind-keymaps))
        (rapid-package--tl-extend! tl (rapid-package--codegen-unbind-forms unbind-keys))
        (rapid-package--tl-extend! tl (rapid-package--codegen-hook-forms hooks))
        (rapid-package--tl-extend! tl (rapid-package--codegen-with-forms with-blocks pkg-name "rapid-package--with"))
        (when diminish-mode
          (rapid-package--tl-append!
           tl `(with-eval-after-load 'diminish (diminish ',diminish-mode))))
        (when delight-spec
          (rapid-package--tl-append!
           tl `(with-eval-after-load 'delight (delight ',delight-spec))))
        (rapid-package--tl-concat! (plist-get buckets :config-pre) tl))

      ;; :config-post
      (when config-body
        (rapid-package--codegen-bucket-extend! buckets :config-post config-body))

      ;; :mode-enable / :mode-disable
      (when modes-enable
        (dolist (m modes-enable)
          (rapid-package--codegen-bucket-append! buckets :config-post `(,m 1))))
      (when modes-disable
        (dolist (m modes-disable)
          (rapid-package--codegen-bucket-append! buckets :config-post `(,m -1))))

      ;; extension expanders
      (rapid-package--codegen-expanders expanders buckets p pkg-name 'package)

      ;; assemble config block
      (let* ((config-tl (plist-get buckets :config-pre))
             (_ (rapid-package--tl-concat! config-tl (plist-get buckets :config-post)))
             (config-forms (rapid-package--tl-value config-tl))
             (after* (when after (if (listp after) after (list after))))
             (top-tl (rapid-package--tl-new))
             (config-block
              (cond
               (after*
                (when config-forms
                  (let ((wrapped config-forms))
                    (dolist (feat (reverse after*))
                      (setq wrapped
                            `((with-eval-after-load ',feat
                                ,(rapid-package--wrap-body wrapped)))))
                    (car wrapped))))
               (demand-p
                (rapid-package--tl-append! top-tl `(require ',pkg-name))
                (rapid-package--tl-extend! top-tl config-forms)
                nil)
               ((not defer-p)
                ;; Non-deferred: always emit require.
                ;; When config-forms is nil (e.g. bare :ensure t with no other
                ;; keywords) we still need to require the package — matching
                ;; use-package's behaviour where absence of deferral triggers
                ;; means the package is loaded immediately.
                (rapid-package--tl-append! top-tl `(require ',pkg-name nil t))
                (rapid-package--tl-extend! top-tl config-forms)
                nil)
               (t
                (when config-forms
                  `(with-eval-after-load ',pkg-name
                     ,(rapid-package--wrap-body config-forms)))))))
        (rapid-package--tl-concat!
         top-tl
         (let ((flat-tl (rapid-package--tl-new)))
           (rapid-package--tl-extend!
            flat-tl (rapid-package--codegen-flatten-buckets buckets bucket-order))
           flat-tl))
        (when config-block
          (rapid-package--tl-append! top-tl config-block))
        `(progn ,@(rapid-package--tl-value top-tl))))))

;;; Conf codegen

(defun rapid-package--codegen-conf (cat expanders p)
  "Return the body form for conf category CAT.
EXPANDERS is the current `rapid-package--expanders' alist.
P is the normalized plist from `rapid-package-dsl-parse'.

Condition wrapping is handled by the caller (`rapid-package--expand-conf')."
  (let ((variable-pairs         (plist-get p :variable))
        (variable-default-pairs (plist-get p :variable-default))
        (custom-pairs           (plist-get p :custom))
        (custom-faces           (plist-get p :custom-face))
        (hooks                  (plist-get p :hook))
        (bindings               (plist-get p :bind))
        (unbind-keys            (plist-get p :unbind))
        (modes                  (plist-get p :mode))
        (modes-enable           (plist-get p :mode-enable))
        (modes-disable          (plist-get p :mode-disable))
        (interpreters           (plist-get p :interpreter))
        (magics                 (plist-get p :magic))
        (init-body              (plist-get p :init))
        (require-features       (plist-get p :require))
        (config-body            (plist-get p :config))
        (after-features         (plist-get p :after))
        (with-blocks            (plist-get p :with))
        (load-path-dirs         (plist-get p :load-path))
        (env-pairs              (plist-get p :env))
        (env-path-entries       (plist-get p :env-path)))

    (let* ((buckets      (rapid-package--codegen-bucket-new '(:init :body)))
           (bucket-order '(:init :body)))

      ;; :init bucket
      (dolist (dir load-path-dirs)
        (rapid-package--codegen-bucket-append!
         buckets :init `(add-to-list 'load-path ,dir)))
      (when env-pairs
        (rapid-package--codegen-bucket-extend!
         buckets :init (rapid-package--codegen-env-forms env-pairs)))
      (when env-path-entries
        (rapid-package--codegen-bucket-extend!
         buckets :init (rapid-package--codegen-env-path-forms env-path-entries)))
      (when init-body
        (rapid-package--codegen-bucket-append! buckets :init `(progn ,@init-body)))

      ;; :require
      (when require-features
        (dolist (feature require-features)
          (rapid-package--codegen-bucket-append!
           buckets :init `(require ',feature))))

      ;; :config - extension registrations (add-keyword etc.) are moved to :init
      ;; so they run before subsequent macro expansions; remaining forms -> :body.
      (let ((def-tl (rapid-package--tl-new)))
        (when config-body
          (let ((imm-tl (rapid-package--tl-new)))
            (dolist (form config-body)
              (rapid-package--tl-append!
               (if (and (listp form)
                        (memq (car form)
                              '(rapid-package-add-keyword
                                rapid-package-add-rewriter
                                rapid-package-add-expander
                                rapid-package-add-handler)))
                   imm-tl def-tl)
               form))
            (rapid-package--tl-concat! (plist-get buckets :init) imm-tl)))

        ;; :body bucket - built regardless of :config presence.
        (let ((body-tl (rapid-package--tl-new)))
          (rapid-package--tl-extend! body-tl (rapid-package--codegen-variable-forms variable-pairs))
          (rapid-package--tl-extend! body-tl (rapid-package--codegen-variable-forms variable-default-pairs t))
          (rapid-package--tl-extend! body-tl (rapid-package--codegen-custom-forms custom-pairs))
          (rapid-package--tl-extend! body-tl (rapid-package--codegen-custom-face-forms custom-faces))
          (rapid-package--tl-extend! body-tl (rapid-package--codegen-hook-forms hooks))
          (rapid-package--tl-extend! body-tl (rapid-package--codegen-bind-forms bindings))
          (rapid-package--tl-extend! body-tl (rapid-package--codegen-unbind-forms unbind-keys))
          (rapid-package--tl-extend! body-tl (rapid-package--codegen-with-forms with-blocks cat "rapid-package--with-conf"))
          ;; :mode, :interpreter, :magic
          (dolist (pat modes)
            (rapid-package--tl-append! body-tl `(add-to-list 'auto-mode-alist ',pat)))
          (dolist (interp interpreters)
            (rapid-package--tl-append! body-tl `(add-to-list 'interpreter-mode-alist ',interp)))
          (dolist (pat magics)
            (let ((entry (if (consp pat) pat (cons pat cat))))
              (rapid-package--tl-append! body-tl `(add-to-list 'magic-mode-alist ',entry))))
          ;; :mode-enable, :mode-disable
          (dolist (m modes-enable)
            (rapid-package--tl-append! body-tl `(,m 1)))
          (dolist (m modes-disable)
            (rapid-package--tl-append! body-tl `(,m -1)))
          (rapid-package--tl-concat! body-tl def-tl)
          (rapid-package--tl-concat! (plist-get buckets :body) body-tl)))

      ;; extension expanders
      (rapid-package--codegen-expanders expanders buckets p cat 'conf)

      ;; :after - wrap :body contents in with-eval-after-load, innermost first.
      ;; (:after (foo bar)) -> (with-eval-after-load 'foo (with-eval-after-load 'bar BODY))
      (when after-features
        (let* ((body-forms (rapid-package--tl-value (plist-get buckets :body)))
               (wrapped
                (seq-reduce
                 (lambda (form feat)
                   `(with-eval-after-load ',feat ,form))
                 (reverse after-features)
                 `(progn ,@body-forms))))
          (setcar (cdr (plist-member buckets :body))
                  (rapid-package--tl-from (list wrapped)))))

      ;; assemble
      (let ((top-forms (rapid-package--codegen-flatten-buckets buckets bucket-order)))
        `(progn ,@top-forms)))))

(provide 'rapid-package-codegen)

;;; rapid-package-codegen.el ends here
