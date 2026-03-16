;;; rapid-package-dsl-parse.el --- Custom parsers for rapid-package DSL -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cottontailia

;; Author: Cottontailia
;; Additional-Author: AI Assistant
;; URL: https://github.com/cottontailia/rapid-package
;; License: CC0

;;; Commentary:

;; Custom parser functions for use with `rapid-package-dsl-parse'.
;;
;; Each function handles one schema keyword and accumulates its IR
;; using the tail-tracked list protocol:
;;
;;   (PARSER-FN ITEM ARGS CURRENT-KEY CURRENT-ACC) -> (NEW-ACC . REMAINING-ARGS)
;;
;; All keyword-specific logic lives here; the core parser in
;; rapid-package-dsl.el remains generic and syntax-focused.
;;
;; Parsers and their finalize functions (for (PARSER . FINALIZE) schema):
;;   :bind        - parse-bind        / finalize-bind
;;   :unbind      - parse-unbind      / finalize-unbind
;;   :hook        - parse-hook        (bare; finalize = tl-value)
;;   :with        - parse-with        (bare; finalize = tl-value)
;;   :mode        - parse-mode        (bare; finalize = tl-value)
;;   :interpreter - parse-interpreter (bare; finalize = tl-value)
;;   :magic       - parse-magic       (bare; finalize = tl-value)
;;
;; Private helpers follow the naming convention:
;;   rapid-package-dsl--<keyword>-<role>

;;; Code:

(require 'cl-lib)
(require 'rapid-package-tl)
(require 'rapid-package-dsl)

;;; Normalization Helpers (custom-parser specific)
;; These depend on rapid-package-dsl--normalize-pair (defined in dsl.el)
;; and are used only by the custom parsers in this file.

(defun rapid-package-dsl--normalize-binding (entry)
  "Normalize a binding ENTRY to plist format.
Handles (key . cmd), (key cmd), (key cmd \"doc\").
CMD may be a plain symbol or #\\='SYMBOL / \\='SYMBOL form.

Note: (KEY . #\\='CMD) is read as (KEY function CMD) — a proper 3-element
list, not a dotted pair — so we detect and handle this case explicitly.

Returns: (:key KEY :command COMMAND [:description DOC])."
  ;; Detect (KEY function CMD) or (KEY quote CMD) — the reader expansions
  ;; of (KEY . #'CMD) and (KEY . 'CMD).  They are proper 3-element lists
  ;; that must be treated as dotted pairs semantically.
  ;;
  ;; Guard: entry must be a proper list of length 3 before calling cddr.
  ;; Improper lists like (KEY . SYMBOL) have a non-list cdr, and calling
  ;; cddr on them would signal a wrong-type-argument error.
  (if (and (consp entry)
           (listp (cdr entry))    ;; proper list guard: cdr must be a list
           (consp (cddr entry))   ;; at least 3 elements
           (null (cdddr entry))   ;; exactly 3 elements
           (memq (cadr entry) '(function quote))
           (symbolp (caddr entry)))
      (list :key (car entry) :command `(,(cadr entry) ,(caddr entry)))
    (rapid-package-dsl--normalize-pair entry :key :command)))

(defun rapid-package-dsl--normalize-fn (raw-fn context)
  "Normalize RAW-FN to a plain function symbol.

Accepted inputs:
  SYMBOL            -> SYMBOL   (used as-is)
  (function SYMBOL) -> SYMBOL   (#\\='foo unwrapped)
  (quote SYMBOL)    -> SYMBOL   (\\='foo unwrapped)

Lambda forms are rejected.  CONTEXT is used in the error message."
  (cond
   ((symbolp raw-fn)
    raw-fn)
   ((and (consp raw-fn)
         (memq (car raw-fn) '(function quote))
         (symbolp (cadr raw-fn))
         (null (cddr raw-fn)))
    (cadr raw-fn))
   (t
    (error "DSL syntax error: %s function must be a symbol or #\\='SYMBOL, got: %S"
           context raw-fn))))

(defun rapid-package-dsl--normalize-hook (entry)
  "Normalize a hook ENTRY to plist format.

Handles (mode . fn), (mode fn), ((m1 m2) . fn), ((m1 m2) fn).
FN is normalized via `rapid-package-dsl--normalize-fn'.

Returns:
  - Single mode:    (:mode MODE :function FN)
  - Multiple modes: (:modes (MODE1 MODE2) :function FN)."
  (cond
   ((and (consp entry) (listp (car entry)))
    (let* ((modes  (car entry))
           (raw-fn (if (listp (cdr entry)) (cadr entry) (cdr entry)))
           (fn     (rapid-package-dsl--normalize-fn raw-fn ":hook")))
      (list :modes modes :function fn)))
   ((consp entry)
    (let* ((raw-fn (if (listp (cdr entry)) (cadr entry) (cdr entry)))
           (fn     (rapid-package-dsl--normalize-fn raw-fn ":hook")))
      (list :mode (car entry) :function fn)))
   (t
    (error "DSL syntax error: invalid hook entry: %S" entry))))

;;; Group-list Infrastructure (used by :bind and :unbind parsers)

(defun rapid-package-dsl--parse-map-spec (item)
  "Parse a (:map MAPS BINDINGS...) ITEM and return (MAPS . BINDINGS).
MAPS may be a single symbol or a list of symbols."
  (let ((rest (cdr item)) maps)
    (unless rest
      (error "DSL syntax error: :map requires at least one keymap"))
    (cond
     ((and (listp (car rest)) (cl-every #'symbolp (car rest)))
      (setq maps (pop rest)))
     ((symbolp (car rest))
      (while (and rest (symbolp (car rest)))
        (push (pop rest) maps))
      (setq maps (nreverse maps)))
     (t
      (error "DSL syntax error: invalid :map specification: %S" item)))
    (cons maps rest)))

(defun rapid-package-dsl--find-or-create-group (group-tl maps bindings-key)
  "Return the group plist for MAPS in GROUP-TL, creating it if absent.
BINDINGS-KEY is :bind or :unbind.  The group plist has the form
\(:map MAPS BINDINGS-KEY TL).  Groups are appended in insertion order."
  (let ((found (cl-find-if
                (lambda (g) (equal (plist-get g :map) maps))
                (rapid-package--tl-value group-tl))))
    (or found
        (let ((g (list :map maps bindings-key (rapid-package--tl-new))))
          (rapid-package--tl-append! group-tl g)
          g))))

(defun rapid-package-dsl--finalize-groups (groups bindings-key)
  "Convert tail-tracked group-list GROUPS to the external IR format.
Each group's BINDINGS-KEY sub-list is converted from a tl to a plain list."
  (mapcar (lambda (g)
            (list :map (plist-get g :map)
                  bindings-key
                  (rapid-package--tl-value (plist-get g bindings-key))))
          (rapid-package--tl-value groups)))

(defun rapid-package-dsl--finalize-bind (acc)
  "Finalize :bind ACC — convert group-list tl to external IR with :bind."
  (rapid-package-dsl--finalize-groups acc :bind))

(defun rapid-package-dsl--finalize-unbind (acc)
  "Finalize :unbind ACC — convert group-list tl to external IR with :unbind."
  (rapid-package-dsl--finalize-groups acc :unbind))



(defun rapid-package-dsl-parse-bind (item args _current-key current-acc)
  "Parse a :bind ITEM and accumulate into CURRENT-ACC.

ITEM may be:
  - A single binding form: (KEY COMMAND ...)
  - A list of binding/map forms: ((KEY CMD ...) (:map MAPS ...) ...)
  - A (:map MAPS ...) form

Returns (NEW-ACC . REMAINING-ARGS).

IR structure (external, after finalization):

  ((:map (MAPS...) :bind (BINDINGS...)) ...)

Global bindings use :map ().

Bindings targeting the same MAPS are merged.
Insertion order of both groups and bindings is preserved.

CURRENT-ACC is either nil (first call) or the internal group-list
tail-tracked structure from a prior call.  Callers should treat it
as opaque; `rapid-package-dsl-parse' finalizes it after the last
item via `rapid-package-dsl--finalize-groups'."
  ;; On the very first call current-acc is nil; initialize the group list then.
  (let ((groups (or current-acc (rapid-package--tl-new))))
    (cl-labels
        ((binding-p (obj)
           (and (consp obj)
                (or (stringp (car obj))
                    (vectorp (car obj)))))

         (add-binding (maps b)
           (unless (binding-p b)
             (error "DSL syntax error: invalid :bind entry %S" b))
           (let* ((normalized (rapid-package-dsl--normalize-binding b))
                  (raw-cmd    (plist-get normalized :command))
                  ;; Normalize command:
                  ;;   symbol       -> kept as-is  (bare symbol)
                  ;;   (function S) -> S           (#'S unwrapped)
                  ;;   (quote S)    -> S           ('S unwrapped)
                  ;;   string       -> kept as-is  (key-translation etc.)
                  ;;   other        -> error
                  (cmd (cond
                        ((symbolp raw-cmd) raw-cmd)
                        ((stringp raw-cmd) raw-cmd)
                        ((and (consp raw-cmd)
                              (memq (car raw-cmd) '(function quote))
                              (symbolp (cadr raw-cmd))
                              (null (cddr raw-cmd)))
                         (cadr raw-cmd))
                        (t
                         (error "DSL syntax error: :bind command must be a symbol, #\\='SYMBOL, or string: %S"
                                raw-cmd))))
                  (group (rapid-package-dsl--find-or-create-group
                          groups (or maps '()) :bind)))
             (rapid-package--tl-append!
              (plist-get group :bind)
              (plist-put (copy-sequence normalized) :command cmd))))

         (consume-map-form (map-form)
           (pcase-let* ((`(,maps . ,rest)
                         (rapid-package-dsl--parse-map-spec map-form)))
             (unless rest
               (error "DSL syntax error: :map requires at least one binding"))
             (dolist (b rest)
               (when (and (consp b) (eq (car b) :map))
                 (error "DSL syntax error: nested :map not allowed"))
               (add-binding maps b))))

         (consume-container (lst)
           (dolist (elem lst)
             (cond
              ((and (consp elem) (eq (car elem) :map))
               (consume-map-form elem))
              ((binding-p elem)
               (add-binding '() elem))
              (t
               (error "DSL syntax error: invalid :bind entry: %S" lst))))))

      (cond
       ((and (consp item) (eq (car item) :map))
        (consume-map-form item))
       ((binding-p item)
        (add-binding '() item))
       ((listp item)
        (consume-container item))
       (t
        (error "DSL syntax error: invalid :bind entry: %S" item)))

      (cons groups args))))

;;; :unbind parser

(defun rapid-package-dsl-parse-unbind (item args _current-key current-acc)
  "Parse a :unbind ITEM and accumulate into CURRENT-ACC.

ITEM may be:
  - A single key (string or vector)
  - A list of keys
  - A (:map MAPS ...) form

Returns (NEW-ACC . REMAINING-ARGS).

IR structure (external, after finalization):

  ((:map (MAPS...) :unbind (KEYS...)) ...)

Global unbindings use :map ().

Keys targeting the same MAPS are merged.
Insertion order of both groups and keys is preserved."
  (let ((groups (or current-acc (rapid-package--tl-new))))
    (cl-labels
        ((valid-key-p (key)
           (or (stringp key) (vectorp key)))

         (add-key (maps key)
           (unless (valid-key-p key)
             (error "DSL syntax error: invalid :unbind entry %S" key))
           (let ((group (rapid-package-dsl--find-or-create-group
                         groups (or maps '()) :unbind)))
             (rapid-package--tl-append! (plist-get group :unbind) key))))

      (cond
       ((and (consp item) (eq (car item) :map))
        (pcase-let* ((`(,maps . ,rest)
                      (rapid-package-dsl--parse-map-spec item)))
          (unless rest
            (error "DSL syntax error: :map requires at least one key"))
          (dolist (k rest)
            (when (and (consp k) (eq (car k) :map))
              (error "DSL syntax error: nested :map not allowed"))
            (add-key maps k))))

       ((and (listp item)
             (cl-every #'valid-key-p item))
        (dolist (k item)
          (add-key '() k)))

       ((valid-key-p item)
        (add-key '() item))

       (t
        (error "DSL syntax error: invalid :unbind entry %S" item)))

      (cons groups args))))

;;; :hook parser

(defun rapid-package-dsl-parse-hook (item args _current-key current-acc)
  "Parse a :hook ITEM and accumulate into CURRENT-ACC.

Accepts the following forms (repeated :hook keywords merge):

  Single entry:
    :hook (prog-mode . my-fn)             ; dotted
    :hook (prog-mode my-fn)              ; list
    :hook ((prog-mode text-mode) . my-fn) ; multi-mode dotted
    :hook ((prog-mode text-mode) my-fn)  ; multi-mode list

  Container list (multiple entries at once):
    :hook ((prog-mode . my-fn) (text-mode . other-fn))

FN must be a plain symbol or a #\\='SYMBOL form.
Lambda forms are not accepted; use :init/:config for anonymous hooks.

Returns (NEW-ACC . REMAINING-ARGS).

IR: a flat list of entry plists after finalization:
  ((:mode MODE :function FN) ... (:modes (M1 M2) :function FN) ...)"
  (let ((tl (or current-acc (rapid-package--tl-new))))
    (cl-labels
        ((single-entry-p (obj)
           ;; Non-nil if OBJ is a single hook entry (not a container list).
           ;; (mode . fn), (mode fn), ((m1 m2) . fn), ((m1 m2) fn)
           (and (consp obj)
                (or (symbolp (car obj))
                    (and (listp (car obj))
                         (cl-every #'symbolp (car obj))))))

         (add-entry (e)
           (rapid-package--tl-append!
            tl (rapid-package-dsl--normalize-hook e))))

      (cond
       ;; Container: a list whose elements are themselves entries
       ((and (listp item)
             (not (single-entry-p item))
             (cl-every #'single-entry-p item))
        (dolist (e item) (add-entry e)))

       ;; Single entry
       ((single-entry-p item)
        (add-entry item))

       (t
        (error "DSL syntax error: invalid :hook entry: %S" item))))

    (cons tl args)))

;;; :with parser

;; IR for a single :with block (after normalization):
;;
;;   (:kind KIND :mode MODE :hook HOOK :map MAP :id ID
;;    :local ENTRIES :bind PAIRS :unbind KEYS)
;;
;; KIND is one of:
;;   :mode  - MODE was a plain mode symbol (e.g. python-mode)
;;            HOOK = MODE-hook, MAP = MODE-map
;;            :local, :bind, :unbind all allowed
;;   :hook  - MODE was a *-hook symbol (e.g. python-mode-hook)
;;            HOOK = MODE, MAP = nil
;;            only :local allowed; :bind and :unbind are errors
;;   :map   - MODE was a *-map symbol (e.g. python-mode-map)
;;            HOOK = nil, MAP = MODE
;;            only :bind and :unbind allowed; :local is an error
;;            no defun/add-hook generated; keymap forms emitted directly
;;
;; :id is the explicit ID symbol, or nil when omitted.
;; :local entries use (:variable VAR :value VAL [:description DOC]) format.
;; VAL in :local has already been processed by `rapid-package-dsl-quote'.

(defun rapid-package-dsl--with-normalize-mode (mode)
  "Return a plist (:kind KIND :hook HOOK :map MAP) for MODE symbol.
KIND is :mode, :hook, or :map depending on MODE's suffix:
  no suffix        -> :mode  HOOK=MODE-hook MAP=MODE-map
  -hook suffix     -> :hook  HOOK=MODE     MAP=nil
  -functions suffix -> :hook  HOOK=MODE     MAP=nil
  -map suffix      -> :map   HOOK=nil      MAP=MODE"
  (let ((name (symbol-name mode)))
    (cond
     ((string-suffix-p "-hook" name)
      (list :kind :hook :hook mode :map nil))
     ((string-suffix-p "-functions" name)
      (list :kind :hook :hook mode :map nil))
     ((string-suffix-p "-map" name)
      (list :kind :map :hook nil :map mode))
     (t
      (list :kind :mode
            :hook (intern (concat name "-hook"))
            :map  (intern (concat name "-map")))))))

(defun rapid-package-dsl--with-normalize-local (raw-pairs)
  "Normalize RAW-PAIRS to plist format.
Uses `rapid-package-dsl--normalize-alist-item'.
Each pair may be (VAR . VAL), (VAR VAL), or (VAR VAL \"DOC\")."
  (mapcar #'rapid-package-dsl--normalize-alist-item raw-pairs))

(defun rapid-package-dsl--with-desugar-flat (items)
  "Convert flat KEY VAL KEY VAL ... ITEMS to a dotted-pair alist.
ITEMS is the cdr of a (:local ...) or (:bind ...) subform.
Signals an error on odd item count."
  (when (cl-oddp (length items))
    (error "DSL syntax error: :with subform has odd number of items: %S" items))
  (let ((tl (rapid-package--tl-new)))
    (while items
      (rapid-package--tl-append! tl (cons (pop items) (pop items))))
    (rapid-package--tl-value tl)))

(defun rapid-package-dsl--with-parse-subforms (subforms mode-sym kind)
  "Parse SUBFORMS into a plist (:local PAIRS :calls FNS :bind PAIRS :unbind KEYS).
SUBFORMS is a list of (:local ...), (:hook ...), (:bind ...), (:unbind ...)
sublists.  KIND is :mode, :hook, or :map (from
`rapid-package-dsl--with-normalize-mode').  MODE-SYM is the original mode
symbol, used in error messages.

Allowed subforms by KIND:
  :mode - :local, :hook, :bind, :unbind
  :hook - :local and :hook only
  :map  - :bind and :unbind only

(:hook FN ...) adds (FN) calls to the generated defun body.
Each FN must be a plain function symbol or #\\='SYMBOL form."
  (let ((local-pairs (rapid-package--tl-new))
        (call-fns (rapid-package--tl-new))
        (bind-pairs (rapid-package--tl-new))
        (unbind-keys (rapid-package--tl-new)))
    (dolist (sub subforms)
      (unless (and (consp sub) (keywordp (car sub)))
        (error "DSL syntax error: :with subform must start with :local, :hook, :bind, or :unbind, got: %S" sub))
      (pcase (car sub)
        (:local
         (when (eq kind :map)
           (error "DSL syntax error: :with :local is not allowed for a keymap (*-map) symbol: %S"
                  mode-sym))
         (let* ((rest (cdr sub))
                (pairs (if (and rest (consp (car rest)) (cl-every #'consp rest))
                           rest
                         (rapid-package-dsl--with-desugar-flat rest))))
           ;; rapid-package-dsl--with-normalize-local returns a fresh list (mapcar)
           (rapid-package--tl-extend! local-pairs
                                      (rapid-package-dsl--with-normalize-local pairs))))
        (:hook
         (when (eq kind :map)
           (error "DSL syntax error: :with :hook is not allowed for a keymap (*-map) symbol: %S"
                  mode-sym))
         (let ((fns (cdr sub)))
           (unless fns
             (error "DSL syntax error: :with :hook requires at least one function symbol"))
           (dolist (fn fns)
             (rapid-package--tl-append! call-fns
                                        (rapid-package-dsl--normalize-fn fn ":with :hook")))))
        (:bind
         (when (eq kind :hook)
           (error "DSL syntax error: :with :bind is not allowed for a hook (*-hook) symbol: %S"
                  mode-sym))
         (let* ((rest (cdr sub))
                (pairs (if (and rest (consp (car rest)) (cl-every #'consp rest))
                           rest
                         (rapid-package-dsl--with-desugar-flat rest))))
           ;; mapcar returns a fresh list, safe to pass ownership
           (rapid-package--tl-extend!
            bind-pairs
            (mapcar (lambda (pair)
                      (let* ((normalized (rapid-package-dsl--normalize-binding pair))
                             (raw-cmd    (plist-get normalized :command))
                             (cmd (cond
                                   ((symbolp raw-cmd) raw-cmd)
                                   ((stringp raw-cmd) raw-cmd)
                                   ((and (consp raw-cmd)
                                         (memq (car raw-cmd) '(function quote))
                                         (symbolp (cadr raw-cmd))
                                         (null (cddr raw-cmd)))
                                    (cadr raw-cmd))
                                   (t
                                    (error "DSL syntax error: :with :bind command must be a symbol, #\\'SYMBOL, or string: %S"
                                           raw-cmd)))))
                        (plist-put normalized :command cmd)))
                    pairs))))
        (:unbind
         (when (eq kind :hook)
           (error "DSL syntax error: :with :unbind is not allowed for a hook (*-hook) symbol: %S"
                  mode-sym))
         (let* ((rest (cdr sub))
                (keys (cond
                       ((and rest (listp (car rest))
                             (cl-every (lambda (k) (or (stringp k) (vectorp k)))
                                       (car rest)))
                        (car rest))
                       ((cl-every (lambda (k) (or (stringp k) (vectorp k))) rest)
                        rest)
                       (t
                        (error "DSL syntax error: :with :unbind keys must be strings or vectors: %S"
                               rest)))))
           ;; keys might reference existing data (car rest or rest), must copy
           (rapid-package--tl-extend! unbind-keys (copy-sequence keys))))
        (_
         (error "DSL syntax error: unknown :with subform keyword %S" (car sub)))))
    (list :local (rapid-package--tl-value local-pairs)
          :calls (rapid-package--tl-value call-fns)
          :bind (rapid-package--tl-value bind-pairs)
          :unbind (rapid-package--tl-value unbind-keys))))

(defun rapid-package-dsl--with-parse-block (mode-sym id-sym subforms)
  "Return a normalized :with IR plist for MODE-SYM with optional ID-SYM.
SUBFORMS is a list of (:local ...), (:hook ...), (:bind ...),
(:unbind ...) sublists.

Simplified IR structure (only non-nil values included):
  (:target MODE [:id ID] [:local PAIRS] [:hook FNS]
                [:bind PAIRS] [:unbind KEYS])
  
The kind, hook name, and map name are derived from :target at codegen time."
  (let* ((sub-ir (rapid-package-dsl--with-parse-subforms 
                  subforms mode-sym
                  (plist-get (rapid-package-dsl--with-normalize-mode mode-sym) :kind)))
         (result (list :target mode-sym)))
    ;; Add optional fields only if they have values
    (when id-sym
      (setq result (plist-put result :id id-sym)))
    (when-let ((local (plist-get sub-ir :local)))
      (setq result (plist-put result :local local)))
    (when-let ((hooks (plist-get sub-ir :calls)))
      (setq result (plist-put result :hook hooks)))
    (when-let ((bindings (plist-get sub-ir :bind)))
      (setq result (plist-put result :bind bindings)))
    (when-let ((unbindings (plist-get sub-ir :unbind)))
      (setq result (plist-put result :unbind unbindings)))
    result))

(defun rapid-package-dsl-parse-with (item args _current-key current-acc)
  "Parse a :with ITEM and accumulate into CURRENT-ACC.

Accepts two forms:

  Shorthand (MODE is the first positional item; ID and subforms follow in ARGS):
    :with MODE     [ID] (:local ...) (:bind ...) (:unbind ...)
    :with MODE-hook [ID] (:local ...)
    :with MODE-map  [ID] (:bind ...) (:unbind ...)

  Canonical (ITEM = a list of blocks):
    :with ((MODE [ID] (:local ...) (:bind ...) ...) ...)

The optional ID is a non-keyword, non-list symbol immediately after MODE.
Multiple :with occurrences are merged.

Behavior by MODE suffix:
  plain  -> :local, :bind, :unbind allowed; generates defun + add-hook
  *-hook -> :local only; generates defun + add-hook
  *-map  -> :bind and :unbind only; keymap forms emitted directly (no defun)

IR per block:
  (:kind KIND :mode MODE :hook HOOK :map MAP :id ID
   :local PAIRS :bind PAIRS :unbind KEYS)"
  (let ((tl (or current-acc (rapid-package--tl-new))))

    (cond
     ;; Canonical form: item is a list of blocks
     ((and (listp item)
           (consp (car item))
           (symbolp (caar item)))
      (dolist (block item)
        (unless (and (consp block) (symbolp (car block)))
          (error "DSL syntax error: :with canonical block must start with a mode symbol: %S" block))
        (let* ((block    (copy-sequence block))
               (mode-sym (pop block))
               (id-sym   (and block
                              (symbolp (car block))
                              (not (keywordp (car block)))
                              (not (consp (car block)))
                              (pop block)))
               (subforms block))
          (rapid-package--tl-append!
           tl (rapid-package-dsl--with-parse-block mode-sym id-sym subforms)))))

     ;; Shorthand: item is the mode symbol
     ((symbolp item)
      (let* ((mode-sym item)
             (id-sym (and args
                          (symbolp (car args))
                          (not (keywordp (car args)))
                          (not (consp (car args)))
                          (pop args)))
             (subforms nil))
        (while (and args (consp (car args)) (keywordp (caar args)))
          (push (pop args) subforms))
        (setq subforms (nreverse subforms))
        (rapid-package--tl-append!
         tl (rapid-package-dsl--with-parse-block mode-sym id-sym subforms))))

     (t
      (error "DSL syntax error: :with expects a mode symbol or list of blocks, got: %S" item)))

    (cons tl args)))

;;; :mode / :interpreter / :magic parsers

(defun rapid-package-dsl--normalize-mode-item (item)
  "Normalize a :mode ITEM to plist format.
Handles:
  - (PATTERN . MODE)             -> (:pattern PAT :mode MODE)
  - (PATTERN MODE)               -> (:pattern PAT :mode MODE)
  - (PATTERN MODE \"description\") -> (:pattern PAT :mode MODE :description DOC)
Bare strings and symbols are not accepted; mode must always be specified.
Returns: (:pattern PATTERN :mode MODE [:description DOC])."
  (if (consp item)
      (rapid-package-dsl--normalize-pair item :pattern :mode)
    (error "DSL syntax error: :mode entry must be (PATTERN MODE) or (PATTERN . MODE), got: %S" item)))

(defun rapid-package-dsl--normalize-interpreter-item (item)
  "Normalize an :interpreter ITEM to plist format.
Handles:
  - (INTERPRETER . MODE)             -> (:interpreter STR :mode MODE)
  - (INTERPRETER MODE)               -> (:interpreter STR :mode MODE)
  - (INTERPRETER MODE \"description\") -> + :description DOC
Bare strings are not accepted; mode must always be specified.
Returns: (:interpreter INTERPRETER :mode MODE [:description DOC])."
  (if (consp item)
      (rapid-package-dsl--normalize-pair item :interpreter :mode)
    (error "DSL syntax error: :interpreter entry must be (INTERP MODE) or (INTERP . MODE), got: %S" item)))

(defun rapid-package-dsl--normalize-magic-item (item)
  "Normalize a :magic ITEM to plist format.
Handles:
  - (MAGIC . MODE)               -> (:magic STR :mode MODE)
  - (MAGIC MODE)                 -> (:magic STR :mode MODE)
  - (MAGIC MODE \"description\")   -> + :description DOC
Bare strings are not accepted; mode must always be specified.
Returns: (:magic MAGIC :mode MODE [:description DOC])."
  (if (consp item)
      (rapid-package-dsl--normalize-pair item :magic :mode)
    (error "DSL syntax error: :magic entry must be (MAGIC MODE) or (MAGIC . MODE), got: %S" item)))

(defun rapid-package-dsl-parse-mode (item args _current-key current-acc)
  "Parse a :mode ITEM and accumulate into CURRENT-ACC.
Accepted ITEM forms:
  (PATTERN MODE)                 - explicit mode
  (PATTERN . MODE)               - dotted pair form
  (PATTERN MODE \"doc\")          - with description
  ((PAT MODE) ...)               - container list of entries
Returns (NEW-ACC . REMAINING-ARGS)."
  (let ((tl (or current-acc (rapid-package--tl-new))))
    (if (and (consp item)
             (consp (car item)))
        (dolist (entry item)
          (rapid-package--tl-append! tl (rapid-package-dsl--normalize-mode-item entry)))
      (rapid-package--tl-append! tl (rapid-package-dsl--normalize-mode-item item)))
    (cons tl args)))

(defun rapid-package-dsl-parse-interpreter (item args _current-key current-acc)
  "Parse an :interpreter ITEM and accumulate into CURRENT-ACC.
Accepted ITEM forms:
  (INTERPRETER MODE)             - explicit mode
  (INTERPRETER . MODE)           - dotted pair form
  (INTERPRETER MODE \"doc\")      - with description
  ((INTERP MODE) ...)            - container list of entries
Returns (NEW-ACC . REMAINING-ARGS)."
  (let ((tl (or current-acc (rapid-package--tl-new))))
    (if (and (consp item)
             (consp (car item)))
        (dolist (entry item)
          (rapid-package--tl-append! tl (rapid-package-dsl--normalize-interpreter-item entry)))
      (rapid-package--tl-append! tl (rapid-package-dsl--normalize-interpreter-item item)))
    (cons tl args)))

(defun rapid-package-dsl-parse-magic (item args _current-key current-acc)
  "Parse a :magic ITEM and accumulate into CURRENT-ACC.
Accepted ITEM forms:
  (MAGIC MODE)                   - explicit mode
  (MAGIC . MODE)                 - dotted pair form
  (MAGIC MODE \"doc\")            - with description
  ((MAGIC MODE) ...)             - container list of entries
Returns (NEW-ACC . REMAINING-ARGS)."
  (let ((tl (or current-acc (rapid-package--tl-new))))
    (if (and (consp item)
             (consp (car item)))
        (dolist (entry item)
          (rapid-package--tl-append! tl (rapid-package-dsl--normalize-magic-item entry)))
      (rapid-package--tl-append! tl (rapid-package-dsl--normalize-magic-item item)))
    (cons tl args)))

;;; :env parser

(defun rapid-package-dsl--normalize-env-item (entry)
  "Normalize an :env ENTRY to plist format.

Handles:
  - (VAR . VALUE)   dotted pair — VALUE may be nil (unset)
  - (VAR VALUE)     two-element list
  - (VAR VALUE DOC) three-element list with description

VAR must be a string.  VALUE must be a string, nil, or a `,EXPR' unquote
form.  nil means unset the variable.  `,EXPR' is evaluated at runtime.
Returns: (:variable VAR :value VALUE [:description DOC])."
  (unless (and (consp entry) (stringp (car entry)))
    (error "DSL syntax error: :env variable name must be a string, got: %S" entry))
  (let ((var (car entry))
        (rest (cdr entry)))
    (cond
     ;; (VAR . nil) or bare (VAR) — unset; skip dsl-quote (nil must stay nil)
     ((null rest)
      (list :variable var :value nil))

     ;; Dotted pair (VAR . VALUE) — cdr is a non-nil atom
     ((not (listp rest))
      (list :variable var :value (rapid-package-dsl-quote rest)))

     ;; 3-element (VAR VALUE DOC)
     ((and (car rest) (cdr rest) (null (cddr rest)))
      (list :variable var
            :value (rapid-package-dsl-quote (car rest))
            :description (cadr rest)))

     ;; 2-element (VAR VALUE)
     ((and rest (null (cdr rest)))
      (list :variable var :value (rapid-package-dsl-quote (car rest))))

     (t (error "DSL syntax error: invalid :env entry format: %S" entry)))))

(defun rapid-package-dsl-parse-env (item args _current-key current-acc)
  "Parse an :env ITEM and accumulate into CURRENT-ACC.

Accepted ITEM forms:
  (VAR . VALUE)                   – single entry, dotted pair
  (VAR VALUE)                     – single entry, list form
  (VAR VALUE DOC)                 – single entry with description
  ((VAR1 . VAL1) (VAR2 VAL2) ...) – multiple entries

VAR must be a string.  VALUE must be a string or nil (nil = unset).
Multiple :env occurrences are merged in declaration order.

Returns (NEW-ACC . REMAINING-ARGS)."
  (let ((tl (or current-acc (rapid-package--tl-new))))
    (cond
     ;; Container list: ((VAR . VAL) ...)
     ((and (consp item)
           (consp (car item))
           (stringp (caar item)))
      (dolist (entry item)
        (rapid-package--tl-append!
         tl (rapid-package-dsl--normalize-env-item entry))))

     ;; Single entry
     ((and (consp item) (stringp (car item)))
      (rapid-package--tl-append!
       tl (rapid-package-dsl--normalize-env-item item)))

     (t
      (error "DSL syntax error: invalid :env value: %S" item)))

    (cons tl args)))

;;; :env-path parser

(defun rapid-package-dsl--env-path-dir-p (x)
  "Return non-nil if X is a valid :env-path directory value.
Accepts strings and unquote forms `(,EXPR)'."
  (or (stringp x)
      (and (consp x) (eq (car x) '\,))))

(defun rapid-package-dsl--env-path-normalize-entry (item)
  "Normalize a single :env-path entry ITEM to a plist.

Accepted forms:
  STRING                          -> (:var \"PATH\" :dir QUOTED :op :prepend)
  ,EXPR                           -> (:var \"PATH\" :dir EXPR   :op :prepend)
  (:prepend DIR)                  -> (:var \"PATH\" :dir QUOTED :op :prepend)
  (:append  DIR)                  -> (:var \"PATH\" :dir QUOTED :op :append)
  (:map VARSYM :prepend DIR)      -> (:var VARNAME :dir QUOTED :op :prepend)
  (:map VARSYM :append  DIR)      -> (:var VARNAME :dir QUOTED :op :append)

DIR may be a string literal or a `,EXPR' unquote form.
Returns a single normalized plist."
  (cond
   ;; Plain string or unquote form -> PATH prepend
   ((rapid-package-dsl--env-path-dir-p item)
    (list :var "PATH" :dir (rapid-package-dsl-quote item) :op :prepend))

   ;; (:prepend DIR) or (:append DIR)
   ((and (consp item)
         (memq (car item) '(:prepend :append))
         (rapid-package-dsl--env-path-dir-p (cadr item))
         (null (cddr item)))
    (list :var "PATH"
          :dir (rapid-package-dsl-quote (cadr item))
          :op  (car item)))

   ;; (:map VARSYM :prepend/:append DIR)
   ((and (consp item)
         (eq (car item) :map))
    (let ((rest (cdr item)))
      (unless (and rest (symbolp (car rest)))
        (error "DSL syntax error: :env-path :map requires a symbol (env var name), got: %S"
               (car rest)))
      (let ((var-name (symbol-name (pop rest))))
        (unless (memq (car rest) '(:prepend :append))
          (error "DSL syntax error: :env-path :map requires :prepend or :append, got: %S"
                 (car rest)))
        (let ((op (pop rest)))
          (unless (and rest (rapid-package-dsl--env-path-dir-p (car rest)))
            (error "DSL syntax error: :env-path dir must be a string or ,EXPR, got: %S"
                   (car rest)))
          (list :var var-name
                :dir (rapid-package-dsl-quote (car rest))
                :op  op)))))

   (t
    (error "DSL syntax error: invalid :env-path entry: %S" item))))

(defun rapid-package-dsl-parse-env-path (item args _current-key current-acc)
  "Parse a :env-path ITEM and accumulate into CURRENT-ACC.

Accepted ITEM forms:
  STRING                         – PATH prepend (shorthand)
  (:prepend STRING)              – PATH prepend
  (:append  STRING)              – PATH append
  (:map VARSYM :prepend STRING)  – other var prepend
  (:map VARSYM :append  STRING)  – other var append
  (ENTRY ...)                    – list of the above (mixed)

Multiple :env-path occurrences are merged in declaration order.

Returns (NEW-ACC . REMAINING-ARGS)."
  (let ((tl (or current-acc (rapid-package--tl-new))))
    (cond
     ;; Single unquote form ,EXPR -> treat as single entry directly
     ((and (consp item) (eq (car item) '\,))
      (rapid-package--tl-append!
       tl (rapid-package-dsl--env-path-normalize-entry item)))

     ;; List of entries: ((ENTRY1) (ENTRY2) ...) or (STRING STRING ...)
     ;; Excludes keyword-headed lists (:prepend/:append/:map) and unquote forms.
     ((and (consp item)
           (not (keywordp (car item))))
      (dolist (entry item)
        (rapid-package--tl-append!
         tl (rapid-package-dsl--env-path-normalize-entry entry))))

     ;; Single entry: string, (:prepend DIR), (:append DIR), (:map ...)
     (t
      (rapid-package--tl-append!
       tl (rapid-package-dsl--env-path-normalize-entry item))))

    (cons tl args)))

(provide 'rapid-package-dsl-parse)

;;; rapid-package-dsl-parse.el ends here
