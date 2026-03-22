;;; rapid-package.el --- Structured package configuration with caching -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cottontailia

;; Author: Cottontailia
;; Additional-Author: AI Assistant
;; URL: https://github.com/cottontailia/rapid-package
;; Version: 0.5.0
;; Package-Requires: ((emacs "29.1") (cl-lib "0.5"))
;; Keywords: convenience, package, tools
;; License: CC0

;;; Commentary:

;; rapid-package provides a structured way to configure Emacs packages
;; with automatic compilation caching and JSON export/import.
;;
;; Features:
;; - Clean DSL for package configuration (use-package compatible)
;; - Automatic byte-compilation with caching
;; - JSON export/import for portability
;; - Hybrid timestamp+hash cache validation
;; - Modern Emacs 29+ keymap APIs (keymap-set, keymap-global-set)
;;
;; Supported keywords:
;;
;; For rapid-package (package configuration):
;;   :preface :ensure :vc :pin :defer :demand :after
;;   :init :require :config :bind :bind* :bind-keymap :unbind :hook
;;   :mode :mode-enable :mode-disable :magic :interpreter :custom :custom-face
;;   :variable :variable-default
;;   :diminish :delight :when :unless
;;   :env :env-path
;;
;; For rapid-package-conf (global configuration):
;;   :variable :variable-default :custom :custom-face :hook :bind :bind-keymap :unbind
;;   :mode :mode-enable :mode-disable :interpreter :magic
;;   :init :require :config :when :unless :load-path
;;   :env :env-path
;;
;; For rapid-package-after (run after package loads):
;;   Similar to rapid-package-conf, but executed after a specified package.
;;
;; Usage:
;;   (rapid-package magit
;;     "Git interface"
;;     :ensure t
;;     :variable ((magit-save-repository-buffers \\='dontask
;;                 "Save buffers without asking"))
;;     :bind (("C-x g" . magit-status))
;;     :custom-face
;;     (magit-diff-added ((t (:foreground \"green\")))))
;;
;;   (rapid-package-conf ui
;;     "UI settings"
;;     :variable (ring-bell-function \\='ignore "Disable bell")
;;     :bind (:map key-translation-map (\"C-h\" . \"<DEL>\"))
;;     :mode-enable (column-number-mode))
;;
;;   (rapid-package-after magit
;;     "Magit post-load config"
;;     :hook (git-commit-mode . my-git-commit-setup))
;;
;;   (rapid-package-load "my-config.el")

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'pp)
(declare-function pp-fill "pp")
(require 'rapid-package-tl)
(require 'rapid-package-dsl)
(require 'rapid-package-dsl-parse)
(require 'rapid-package-codegen)
(require 'rapid-package-fontset)

(defconst rapid-package-version "0.5.0"
  "Current version; used in cache metadata and JSON export.")

;;; Customization

(defgroup rapid-package nil
  "Structured package configuration with caching."
  :group 'convenience
  :prefix "rapid-package-")

(defcustom rapid-package-cache-dir
  (expand-file-name "rapid-cache/" user-emacs-directory)
  "Directory to store compiled and JSON cache files."
  :type 'directory
  :group 'rapid-package)

(defcustom rapid-package-use-hash-validation t
  "If non-nil, use content hash for cache validation.
If nil, only use timestamp (faster but less reliable)."
  :type 'boolean
  :group 'rapid-package)

(defcustom rapid-package-native-compile nil
  "If non-nil, use native compilation instead of byte compilation for cache.
Native compilation produces faster code but requires Emacs to be built
with native compilation support (check with `(featurep \\='native-compile)').
If native compilation is requested but not available, falls back to
byte compilation with a warning."
  :type 'boolean
  :group 'rapid-package)

(defcustom rapid-package-always-ensure nil
  "If non-nil, :ensure t is the default for all packages."
  :type 'boolean
  :group 'rapid-package)

(defcustom rapid-package-always-defer nil
  "If non-nil, :defer t is the default for all packages."
  :type 'boolean
  :group 'rapid-package)

(defcustom rapid-package-json-auto-write nil
  "Control automatic JSON generation when loading an .el file.

When `rapid-package-load' processes a .el file, it can also write a
sibling .json file (same directory, same base name) that is a portable,
human-readable representation of the same configuration.

Possible values:
  nil      Never write JSON automatically (default).
  t        Write JSON whenever the .el file is recompiled (i.e. on change).
  always   Write JSON on every load regardless of compile state.
           **Performance note**: This requires re-parsing the source file
           on every load, which reduces the benefit of caching.
           Recommended for debugging, CI, or documentation generation only.

The written .json is a first-class configuration file: it can be passed
directly to `rapid-package-load' and round-trips losslessly for
rapid-package, rapid-package-conf, and rapid-package-after macros."
  :type '(choice (const :tag "Never (manual only)"   nil)
                 (const :tag "On change (recompile)" t)
                 (const :tag "Always (debug/CI)"     always))
  :group 'rapid-package)

;;; imenu support

;; Regexp fragment matching a bare symbol (package/conf name).
(defconst rapid-package--imenu-sym-regexp "\\_<\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>"
  "Regexp fragment matching a symbol name for use in imenu patterns.")

(defconst rapid-package--imenu-package-regexp
  (concat "^\\s-*("
          (regexp-opt '("rapid-package") t)
          "\\s-+" rapid-package--imenu-sym-regexp)
  "Regexp matching a `rapid-package' declaration for imenu.
Group 2 captures the package name.")

(defconst rapid-package--imenu-conf-regexp
  (concat "^\\s-*("
          (regexp-opt '("rapid-package-conf") t)
          "\\s-+" rapid-package--imenu-sym-regexp)
  "Regexp matching a `rapid-package-conf' declaration for imenu.
Group 2 captures the category name.")

(defconst rapid-package--imenu-after-regexp
  (concat "^\\s-*("
          (regexp-opt '("rapid-package-after") t)
          "\\s-+" rapid-package--imenu-sym-regexp)
  "Regexp matching a `rapid-package-after' declaration for imenu.
Group 2 captures the package name.")

(defconst rapid-package--imenu-fontset-regexp
  (concat "^\\s-*("
          (regexp-opt '("rapid-package-fontset") t)
          "\\s-+" rapid-package--imenu-sym-regexp)
  "Regexp matching a `rapid-package-fontset' declaration for imenu.
Group 2 captures the fontset name.")

(defconst rapid-package--imenu-package-entry
  (list "Packages" rapid-package--imenu-package-regexp 2)
  "Imenu entry for `rapid-package' declarations.")

(defconst rapid-package--imenu-conf-entry
  (list "Conf" rapid-package--imenu-conf-regexp 2)
  "Imenu entry for `rapid-package-conf' declarations.")

(defconst rapid-package--imenu-after-entry
  (list "After" rapid-package--imenu-after-regexp 2)
  "Imenu entry for `rapid-package-after' declarations.")

(defconst rapid-package--imenu-fontset-entry
  (list "Fontsets" rapid-package--imenu-fontset-regexp 2)
  "Imenu entry for `rapid-package-fontset' declarations.")

(defcustom rapid-package-enable-imenu-support nil
  "If non-nil, add rapid-package macros to imenu.

When enabled, `imenu' (and imenu-based tools such as consult-imenu,
helm-imenu, or counsel-imenu) will list the following declarations:
- `rapid-package' under \"Packages\"
- `rapid-package-conf' under \"Conf\"
- `rapid-package-after' under \"After\"
- `rapid-package-fontset' under \"Fontsets\"

This works by adding entries to `lisp-imenu-generic-expression', which
is consulted by `emacs-lisp-mode' when building the imenu index.

This variable must be set BEFORE `rapid-package' is loaded to take
effect.  The idiomatic way is to set it in early-init.el or at the
very top of init.el, before the first `require':

  (setq rapid-package-enable-imenu-support t)
  (require \\='rapid-package)

Alternatively, use Customize to toggle it and then restart Emacs."
  :type 'boolean
  :group 'rapid-package
  :set (lambda (sym value)
         (eval-after-load 'lisp-mode
           (if value
               `(progn
                  (add-to-list 'lisp-imenu-generic-expression
                               ',rapid-package--imenu-package-entry)
                  (add-to-list 'lisp-imenu-generic-expression
                               ',rapid-package--imenu-conf-entry)
                  (add-to-list 'lisp-imenu-generic-expression
                               ',rapid-package--imenu-after-entry)
                  (add-to-list 'lisp-imenu-generic-expression
                               ',rapid-package--imenu-fontset-entry))
             `(progn
                (setq lisp-imenu-generic-expression
                      (delete ',rapid-package--imenu-package-entry
                              lisp-imenu-generic-expression))
                (setq lisp-imenu-generic-expression
                      (delete ',rapid-package--imenu-conf-entry
                              lisp-imenu-generic-expression))
                (setq lisp-imenu-generic-expression
                      (delete ',rapid-package--imenu-after-entry
                              lisp-imenu-generic-expression))
                (setq lisp-imenu-generic-expression
                      (delete ',rapid-package--imenu-fontset-entry
                              lisp-imenu-generic-expression)))))
         (set-default sym value)))

;;; font-lock

(defconst rapid-package--font-lock-keywords
  '(("(\\(rapid-package\\(?:-conf\\|-after\\|-fontset\\)?\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t)))
  "Font-lock rules to highlight rapid-package macros.")

(font-lock-add-keywords 'emacs-lisp-mode rapid-package--font-lock-keywords)

;;; Messaging System

(defun rapid-package--message (context msg &rest args)
  "Display a message with [rapid-package] [CONTEXT] prefix.
CONTEXT is a symbol or string; MSG is the format string, ARGS are its arguments."
  (let ((ctx-str (if (symbolp context) (symbol-name context) context)))
    (apply #'message (concat "[rapid-package] [%s] " msg) ctx-str args)))

(defun rapid-package--warning (context msg &rest args)
  "Display a warning in *Warnings* and echo area.
Prefixes with [rapid-package] [CONTEXT].
CONTEXT is a symbol or string; MSG is the format string, ARGS are
its arguments."
  (let* ((ctx-str (if (symbolp context) (symbol-name context) context))
         (body (apply #'format (concat "[%s] " msg) ctx-str args)))
    (display-warning 'rapid-package body :warning)
    (message "[rapid-package] %s" body)))

(defun rapid-package--abort (context msg &rest args)
  "Signal a user-error with [rapid-package] [CONTEXT] prefix.
Also logs to *Warnings*.
CONTEXT is a symbol or string; MSG is the format string, ARGS are
its arguments."
  (let* ((ctx-str (if (symbolp context) (symbol-name context) context))
         (body (apply #'format (concat "[%s] " msg) ctx-str args)))
    (display-warning 'rapid-package body :error)
    (user-error "[rapid-package] %s" body)))


(defvar rapid-package--expansion-phase nil
  "Non-nil while `rapid-package--expand-file' is processing a .el file.

This variable can be used by extension functions to detect whether they are
being called during the initial expansion phase (when forms are evaluated
for registration purposes) vs. normal evaluation.

Currently not used internally, but reserved for future extensions.")

(defvar rapid-package--loading-file nil
  "File path being processed by `rapid-package--expand-file', or nil.

Non-nil only when a macro is being expanded as part of `rapid-package-load'
processing a .el file.  Used to include file and line information in DSL
error messages.")

(defvar rapid-package--loading-line nil
  "Line number of the form being expanded by `rapid-package--expand-file', or nil.

Corresponds to `rapid-package--loading-file'.")

(defsubst rapid-package--loc ()
  "Return \"FILE:LINE: \" prefix when location context is set, else \"\"."
  (if rapid-package--loading-file
      (format "%s:%d: " rapid-package--loading-file
              (or rapid-package--loading-line 0))
    ""))

;;; Schemas

(defvar rapid-package-schema
  `((:_head . list)        ;; Package name + docstring
    (:ensure . single)     ;; t / nil / package name
    (:vc . single)         ;; Version control spec (Emacs 29+)
    (:pin . single)        ;; Archive name (e.g., "melpa")
    (:defer . flag)        ;; Defer loading
    (:demand . flag)       ;; Force immediate loading
    (:after . list)        ;; Load after these packages
    (:commands . list)     ;; Autoload these commands
    (:init . body)         ;; Execute before loading
    (:require . list)      ;; Require features
    (:config . body)       ;; Execute after loading
    (:preface . body)      ;; Execute at compile time
    (:bind . ,(cons #'rapid-package-dsl-parse-bind
                    #'rapid-package-dsl--finalize-bind))       ;; Key bindings with :map
    (:bind* . ,(cons #'rapid-package-dsl-parse-bind
                     #'rapid-package-dsl--finalize-bind))      ;; Override bindings
    (:bind-keymap . alist) ;; Prefix keymap bindings
    (:unbind . ,(cons #'rapid-package-dsl-parse-unbind
                      #'rapid-package-dsl--finalize-unbind))   ;; Unbind keys
    (:hook . ,#'rapid-package-dsl-parse-hook) ;; Mode hooks (finalize = tl-value)
    (:mode . ,(cons #'rapid-package-dsl-parse-mode
                    #'rapid-package--tl-value))        ;; Auto-mode-alist entries as (:pattern PAT :mode MODE) plists
    (:mode-enable . list)  ;; Modes to enable
    (:mode-disable . list) ;; Modes to disable
    (:interpreter . ,(cons #'rapid-package-dsl-parse-interpreter
                           #'rapid-package--tl-value)) ;; Interpreter-mode-alist entries as (:interpreter STR :mode MODE) plists
    (:magic . ,(cons #'rapid-package-dsl-parse-magic
                     #'rapid-package--tl-value))       ;; Magic-mode-alist entries as (:magic STR :mode MODE) plists
    (:when . single)       ;; Conditional loading
    (:unless . single)     ;; Conditional loading (negated)
    (:when-system . list)  ;; System type check (OR condition)
    (:unless-system . list) ;; NOT (system type OR condition)
    (:when-gui . flag)     ;; GUI environment check
    (:when-tty . flag)     ;; TTY environment check
    (:when-ge . alist)     ;; >= comparisons: ((VAR VAL) ...)
    (:when-gt . alist)     ;; > comparisons: ((VAR VAL) ...)
    (:when-le . alist)     ;; <= comparisons: ((VAR VAL) ...)
    (:when-lt . alist)     ;; < comparisons: ((VAR VAL) ...)
    (:when-eq . alist)     ;; = comparisons: ((VAR VAL) ...)
    (:when-ne . alist)     ;; /= comparisons: ((VAR VAL) ...)
    (:when-p . list)       ;; Predicate checks: (PRED ...)
    (:custom . alist)      ;; Custom variables
    (:custom-face . alist) ;; Custom faces
    (:variable . alist)    ;; Variable settings (3-element doc support)
    (:variable-default . alist) ;; Default value settings
    (:diminish . single)   ;; Hide minor mode from modeline
    (:delight . single)    ;; Customize modeline display
    (:disabled . flag)     ;; Disable this block entirely
    (:load-path . list)    ;; Directories to prepend to load-path
    (:env . ,#'rapid-package-dsl-parse-env) ;; Environment variables (setenv)
    (:env-path . ,#'rapid-package-dsl-parse-env-path) ;; PATH-like var manipulation
    (:with . ,#'rapid-package-dsl-parse-with)) ;; Mode-local setup blocks
  "Schema for rapid-package macro.
Each entry is (KEYWORD . TYPE) where TYPE can be:
  single, list, flag, body, alist,
  PARSER-FN (finalize defaults to `rapid-package--tl-value'),
  or (PARSER-FN . FINALIZE-FN).")

(defvar rapid-package-conf-schema
  `((:_head . list)        ;; Category name + docstring
    (:variable . alist)    ;; Variable settings (3-element doc support)
    (:variable-default . alist) ;; Default value settings
    (:custom . alist)      ;; Custom variables
    (:custom-face . alist) ;; Custom faces
    (:hook . ,#'rapid-package-dsl-parse-hook) ;; Mode hooks (finalize = tl-value)
    (:bind . ,(cons #'rapid-package-dsl-parse-bind
                    #'rapid-package-dsl--finalize-bind))      ;; Key bindings with :map
    (:bind-keymap . alist) ;; Prefix keymap bindings
    (:unbind . ,(cons #'rapid-package-dsl-parse-unbind
                      #'rapid-package-dsl--finalize-unbind))  ;; Unbind keys
    (:mode . ,(cons #'rapid-package-dsl-parse-mode
                    #'rapid-package--tl-value))        ;; Auto-mode-alist entries as (:pattern PAT :mode MODE) plists
    (:mode-enable . list)  ;; Modes to enable
    (:mode-disable . list) ;; Modes to disable
    (:interpreter . ,(cons #'rapid-package-dsl-parse-interpreter
                           #'rapid-package--tl-value)) ;; Interpreter-mode-alist entries as (:interpreter STR :mode MODE) plists
    (:magic . ,(cons #'rapid-package-dsl-parse-magic
                     #'rapid-package--tl-value))       ;; Magic-mode-alist entries as (:magic STR :mode MODE) plists
    (:init . body)         ;; Execute at init
    (:require . list)      ;; Require features
    (:config . body)       ;; Execute at config
    (:after . list)        ;; Apply after these features are loaded
    (:when . single)       ;; Conditional execution
    (:unless . single)     ;; Conditional execution (negated)
    (:when-system . list)  ;; System type check (OR condition)
    (:unless-system . list) ;; NOT (system type OR condition)
    (:when-gui . flag)     ;; GUI environment check
    (:when-tty . flag)     ;; TTY environment check
    (:when-ge . alist)     ;; >= comparisons: ((VAR VAL) ...)
    (:when-gt . alist)     ;; > comparisons: ((VAR VAL) ...)
    (:when-le . alist)     ;; <= comparisons: ((VAR VAL) ...)
    (:when-lt . alist)     ;; < comparisons: ((VAR VAL) ...)
    (:when-eq . alist)     ;; = comparisons: ((VAR VAL) ...)
    (:when-ne . alist)     ;; /= comparisons: ((VAR VAL) ...)
    (:when-p . list)       ;; Predicate checks: (PRED ...)
    (:disabled . flag)     ;; Disable this block entirely
    (:load-path . list)    ;; Directories to prepend to load-path
    (:env . ,#'rapid-package-dsl-parse-env) ;; Environment variables (setenv)
    (:env-path . ,#'rapid-package-dsl-parse-env-path) ;; PATH-like var manipulation
    (:with . ,#'rapid-package-dsl-parse-with)) ;; Mode-local setup blocks
  "Schema for rapid-package-conf macro.
Each entry is (KEYWORD . TYPE) where TYPE can be:
  single, list, flag, body, alist,
  PARSER-FN (finalize defaults to `rapid-package--tl-value'),
  or (PARSER-FN . FINALIZE-FN).")

;;; Extension System

(defvar rapid-package--rewriters nil
  "Alist of plist rewriters registered via `rapid-package-add-rewriter'.
Each entry: (:keyword KW :for FOR :fn FN)
Processed in registration order before macro body expansion.")

(defvar rapid-package--expanders nil
  "Alist of form expanders registered via `rapid-package-add-expander'.
Each entry: (:keyword KW :for FOR :position POS :expander FN)
Processed during macro expansion; FN returns a list of forms.")

;; Valid positions in body (package macro):
;;   :preface :pin :install :init :trigger :config-pre :config-post
;; Valid positions in body (conf macro):
;;   :init :body
;; Position spec: SYMBOL or (:before SYMBOL) or (:after SYMBOL)

(defun rapid-package--apply-rewriters (plist name for)
  "Apply all registered rewriters for FOR to PLIST.
Rewriters are applied in registration order.
NAME is the package/category symbol."
  (let ((result plist))
    (dolist (entry rapid-package--rewriters result)
      (when (or (eq (plist-get entry :for) 'both)
                (eq (plist-get entry :for) for))
        (let ((kw (plist-get entry :keyword)))
          (when (plist-member result kw)
            (setq result
                  (funcall (plist-get entry :fn) result name))))))))

;;;###autoload
(defun rapid-package-add-keyword (keyword schema-type for)
  "Register KEYWORD into the rapid-package schema with SCHEMA-TYPE.

SCHEMA-TYPE: \\='single \\='list \\='flag \\='body \\='alist, or a custom parser function.
FOR: \\='package, \\='conf, or \\='both.

Registers the keyword for DSL parsing only.  To generate code from the
parsed value, also call `rapid-package-add-expander' or
`rapid-package-add-rewriter'.

Extension registrations inside :config bodies are evaluated before
subsequent macro expansions in the same file, so the typical usage is to
call this at top level or inside :config of an early conf block.

Example:
  (rapid-package-add-keyword :straight \\='single \\='package)"
  (let ((schema (pcase for
                  ('package 'rapid-package-schema)
                  ('conf    'rapid-package-conf-schema)
                  ('both    'both)
                  (_ (rapid-package--abort 'extension
                                           "invalid for argument %S (expected 'package, 'conf, or 'both)"
                                           for)))))
    (if (eq schema 'both)
        (progn
          (rapid-package-add-keyword keyword schema-type 'package)
          (rapid-package-add-keyword keyword schema-type 'conf))
      (let ((sym (symbol-value schema)))
        (unless (assq keyword sym)
          (set schema (append sym (list (cons keyword schema-type)))))))))

;;;###autoload
(defun rapid-package-add-rewriter (keyword for fn)
  "Register a plist rewriter triggered when KEYWORD is present.

FN is called when KEYWORD is present in the parsed plist.
Signature: (plist name) -> plist
  PLIST : the full parsed plist
  NAME  : package/category symbol
  Return: the (possibly modified) plist

The caller is fully responsible for the shape of the returned plist.
For example, to alias :straight to :ensure and remove the original:

  (lambda (plist _name)
    (let ((val (plist-get plist :straight)))
      (plist-put (map-delete plist :straight) :ensure val)))

Or to simply add :ensure while keeping :straight:

  (lambda (plist _name)
    (plist-put plist :ensure (plist-get plist :straight)))

FOR: \\='package, \\='conf, or \\='both
Rewriters run in registration order, before macro body expansion.
Multiple rewriters may modify the same keyword; each sees the
result of the previous one."
  ;; Remove existing rewriter for same keyword+for to ensure idempotency
  (setq rapid-package--rewriters
        (cl-remove-if (lambda (entry)
                        (and (eq (plist-get entry :keyword) keyword)
                             (eq (plist-get entry :for) for)))
                      rapid-package--rewriters))
  ;; Add new rewriter
  (setq rapid-package--rewriters
        (append rapid-package--rewriters
                (list (list :keyword keyword :for for :fn fn))))
  keyword)

;;;###autoload
(defun rapid-package-add-expander (keyword for &rest args)
  "Register a form expander for KEYWORD (package developer API).

ARGS is a plist:
  :position  Position spec (default \\=':config-post for package,
                             \\=':body for conf).
             Symbol: appended to that bucket.
             (:before SYM): prepended before SYM bucket.
             (:after  SYM): appended after SYM bucket
                            (same as bare symbol).
  :expander  Function (value name plist) -> list-of-forms.
             Called only when KEYWORD has a non-nil value in plist.
             Must return a LIST of Emacs Lisp forms.

FOR: \\='package, \\='conf, or \\='both

When FOR is \\='both, the same :position is applied to both the package
and conf registrations.  Since valid buckets differ between the two macros
(e.g. :config-post is package-only; :body is conf-only), specifying a
bucket that exists in only one side will cause an error for the other.
In that case, call this function twice: once for \\='package and once for
\\='conf, with appropriate positions for each.

Valid bucket names for rapid-package:
  :preface :pin :install :init :trigger :config-pre :config-post
Valid bucket names for rapid-package-conf: :init :body

Example:
    :position \\='(:before :install)
    :expander (lambda (value name _plist)
                \\=`((straight-use-package \\=',value))))"
  (if (eq for 'both)
      ;; Split into two entries with macro-appropriate default positions
      (progn
        (apply #'rapid-package-add-expander keyword 'package args)
        (apply #'rapid-package-add-expander keyword 'conf    args)
        keyword)
    (let* ((default-pos (if (eq for 'conf) :body :config-post))
           (position  (or (plist-get args :position) default-pos))
           (expander  (plist-get args :expander)))
      (unless expander
        (rapid-package--abort 'extension
                              ":expander argument is required for keyword %S" keyword))
      (unless (functionp expander)
        (rapid-package--abort 'extension
                              ":expander must be a function for keyword %S, got %S"
                              keyword expander))
      ;; Remove existing expander for same keyword+for to ensure idempotency
      (setq rapid-package--expanders
            (cl-remove-if (lambda (entry)
                            (and (eq (plist-get entry :keyword) keyword)
                                 (eq (plist-get entry :for) for)))
                          rapid-package--expanders))
      ;; Add new expander
      (setq rapid-package--expanders
            (append rapid-package--expanders
                    (list (list :keyword keyword :for for
                                :position position :expander expander))))
      keyword)))

;;;###autoload
(defun rapid-package-add-handler (keyword for &rest args)
  "Register a runtime function call for KEYWORD (user API).

ARGS is a plist:
  :position  Same as `rapid-package-add-expander'.
  :handler   Function symbol or lambda. Called at runtime with
             (name plist) when KEYWORD has a non-nil value.
             name  : package/category symbol (quoted)
             plist : parsed plist (quoted)

FOR: \\='package, \\='conf, or \\='both

This is a convenience wrapper around `rapid-package-add-expander'.
The generated call is:
  (HANDLER \\='NAME \\='PLIST)

Example:
  (rapid-package-add-handler :ensure-system-package \\='package
    :position \\='(:before :install)
    :handler  #\\='my/check-system-pkg)"
  (let ((handler  (plist-get args :handler))
        (position (plist-get args :position)))
    (unless handler
      (rapid-package--abort 'extension
                            "rapid-package-add-handler: :handler is required"))
    (apply #'rapid-package-add-expander keyword for
           :expander (lambda (value name plist)
                       (ignore value) ; value already checked non-nil by caller
                       `((,handler ',name ',plist)))
           (when position (list :position position)))))

;;; Helper Functions

(defun rapid-package--parse-head (head-list)
  "Parse :_head list into (name . docstring).

HEAD-LIST should be (NAME [DOCSTRING]).

Returns a cons cell (NAME . DOCSTRING) where DOCSTRING may be nil."
  (cond
   ((null head-list)
    (rapid-package--abort 'parse "Name is required"))
   ((= (length head-list) 1)
    (cons (car head-list) nil))
   ((= (length head-list) 2)
    (let ((name (car head-list))
          (doc (cadr head-list)))
      (unless (stringp doc)
        (rapid-package--abort 'parse "Second argument should be a docstring, got: %S" doc))
      (cons name doc)))
   (t
    (rapid-package--abort 'parse "Too many arguments before keywords: %S" head-list))))

(defun rapid-package--unquote-expr (val)
  "Unwrap (\\, EXPR) to EXPR; return other values unchanged.
Used to resolve unquote markers in IR values at codegen time."
  (if (and (consp val) (eq (car val) '\,))
      (cadr val)
    val))

(defun rapid-package--check-condition (plist)
  "Extract conditional expression from PLIST.

Checks all conditional keywords and combines them with AND.
Returns nil if :disabled is set, combined condition form, or t if no conditions."
  (cond
   ((plist-get plist :disabled) nil)
   (t
    (let ((conditions '()))
      ;; :when - arbitrary condition
      (when-let ((cond (plist-get plist :when)))
        (push cond conditions))
      
      ;; :unless - negated condition
      (when-let ((cond (plist-get plist :unless)))
        (push `(not ,cond) conditions))
      
      ;; :when-system - OR of system types
      (when-let ((systems (plist-get plist :when-system)))
        (push `(memq system-type ',systems) conditions))
      
      ;; :unless-system - NOT (OR of system types)
      (when-let ((systems (plist-get plist :unless-system)))
        (push `(not (memq system-type ',systems)) conditions))
      
      ;; :when-gui - GUI check
      (when (plist-get plist :when-gui)
        (push '(display-graphic-p) conditions))
      
      ;; :when-tty - TTY check
      (when (plist-get plist :when-tty)
        (push '(not (display-graphic-p)) conditions))
      
      ;; :when-ge - >= comparisons
      (when-let ((pairs (plist-get plist :when-ge)))
        (dolist (pair pairs)
          (let ((var (plist-get pair :variable))
                (val (rapid-package--unquote-expr (plist-get pair :value))))
            (push `(>= ,var ,val) conditions))))

      ;; :when-gt - > comparisons
      (when-let ((pairs (plist-get plist :when-gt)))
        (dolist (pair pairs)
          (let ((var (plist-get pair :variable))
                (val (rapid-package--unquote-expr (plist-get pair :value))))
            (push `(> ,var ,val) conditions))))

      ;; :when-le - <= comparisons
      (when-let ((pairs (plist-get plist :when-le)))
        (dolist (pair pairs)
          (let ((var (plist-get pair :variable))
                (val (rapid-package--unquote-expr (plist-get pair :value))))
            (push `(<= ,var ,val) conditions))))

      ;; :when-lt - < comparisons
      (when-let ((pairs (plist-get plist :when-lt)))
        (dolist (pair pairs)
          (let ((var (plist-get pair :variable))
                (val (rapid-package--unquote-expr (plist-get pair :value))))
            (push `(< ,var ,val) conditions))))

      ;; :when-eq - = comparisons
      (when-let ((pairs (plist-get plist :when-eq)))
        (dolist (pair pairs)
          (let ((var (plist-get pair :variable))
                (val (rapid-package--unquote-expr (plist-get pair :value))))
            (push `(= ,var ,val) conditions))))

      ;; :when-ne - /= comparisons
      (when-let ((pairs (plist-get plist :when-ne)))
        (dolist (pair pairs)
          (let ((var (plist-get pair :variable))
                (val (rapid-package--unquote-expr (plist-get pair :value))))
            (push `(/= ,var ,val) conditions))))
      
      ;; :when-p - predicate checks
      (when-let ((preds (plist-get plist :when-p)))
        (dolist (pred preds)
          (push `(,pred) conditions)))
      
      ;; Combine with AND
      (cond
       ((null conditions) t)
       ((= (length conditions) 1) (car conditions))
       (t `(and ,@(nreverse conditions))))))))


;;; Main Macros

(defmacro rapid-package (package &rest args)
  "Configure PACKAGE with ARGS.

PACKAGE can be followed by an optional docstring.

Supported keywords
  :preface BODY...      - Execute before loading (even at compile time)
  :ensure BOOL/PKG      - Install if not present
  :vc SPEC              - Install from version control (Emacs 29+)
  :pin ARCHIVE          - Pin to specific archive (e.g. \"melpa\")
  :defer BOOL           - Defer loading
  :demand BOOL          - Force immediate loading
  :after PACKAGES       - Load after these packages
  :init BODY...         - Execute before loading
  :config BODY...       - Execute after loading
  :variable PAIRS       - Set variables (supports 3-element docstring)
                          Values support ,EXPR for runtime
                          evaluation
  :variable-default     - Set default values (supports 3-element
                          docstring)
                          Values support ,EXPR for runtime
                          evaluation
  :bind BINDINGS        - Key bindings (supports :map KEYMAP ...)
  :bind* BINDINGS       - Override key bindings
  :bind-keymap PAIRS    - Bind prefix keys to keymaps
  :unbind KEYS          - Unbind keys (string or :map format)
  :hook HOOKS           - Mode hooks
  :mode PATTERNS        - Auto-mode-alist entries
  :magic PATTERNS       - Magic-mode-alist entries (buffer content
                          regexp matching)
  :interpreter PATTERNS - Interpreter-mode-alist entries
  :custom VARIABLES     - Custom variables; values support ,EXPR for
                          runtime evaluation
  :custom-face FACES    - Custom faces; values support ,EXPR for
                          runtime evaluation
  :diminish MODE        - Hide minor mode from modeline
  :delight SPEC         - Customize modeline display
  :disabled BOOL        - Disable this block entirely (skip all code
                          generation)
  :load-path DIRS       - Prepend directories to load-path before
                          loading
  :with MODE [ID] SUBS  - Mode-local setup (shorthand); or
                          :with (BLOCKS...)
                          SUBS: (:local VAR VAL ...) (:hook FN ...)
                                (:bind KEY CMD ...) (:unbind KEY ...)
                          :local supports 3-element docstring and
                          ,EXPR values
                          :hook FN calls (FN) in the generated hook
                          function
                          Generates a named hook function +
                          add-hook
  :when CONDITION       - Conditional loading
  :unless CONDITION     - Conditional loading (negated)
  :when-system SYSTEMS  - Load only on specified system types (OR condition)
                          e.g., :when-system (gnu/linux darwin)
  :unless-system SYSTEMS - Load except on specified system types (NAND)
  :when-gui BOOL        - Load only in GUI environment
  :when-tty BOOL        - Load only in TTY environment
  :when-ge PAIRS        - Load when >= comparisons hold: ((VAR VAL) ...)
                          e.g., :when-ge ((emacs-major-version 28))
  :when-gt PAIRS        - Load when > comparisons hold
  :when-le PAIRS        - Load when <= comparisons hold
  :when-lt PAIRS        - Load when < comparisons hold
  :when-eq PAIRS        - Load when = comparisons hold
  :when-ne PAIRS        - Load when /= comparisons hold
  :when-p PREDICATES    - Load when predicates return non-nil
                          e.g., :when-p (daemonp server-running-p)

Example:
  (rapid-package magit
    \"Git interface\"
    :ensure t
    :variable ((magit-save-repository-buffers \\='dontask
                \"Save buffers without asking\"))
    :bind ((\"C-x g\" . magit-status))
    :hook (magit-mode . magit-auto-revert-mode)
    :config
    (setq magit-display-buffer-function
          #\\='magit-display-buffer-fullframe-status-v1))"
  (declare (indent defun))
  (let* ((p (condition-case err
                (rapid-package-dsl-parse (cons package args) rapid-package-schema)
              (error
               (if rapid-package--loading-file
                   (rapid-package--abort 'dsl "%s:%d: %s: %s"
                                         rapid-package--loading-file
                                         rapid-package--loading-line
                                         package
                                         (error-message-string err))
                 (rapid-package--abort 'dsl "%s: %s"
                                       package
                                       (error-message-string err))))))
         (head-parsed (rapid-package--parse-head (plist-get p :_head)))
         (pkg-name  (car head-parsed))
         (condition  (rapid-package--check-condition p)))

    ;; Apply registered rewriters (modifies p before body expansion)
    (setq p (rapid-package--apply-rewriters p pkg-name 'package))

    (rapid-package--expand-package pkg-name condition p)))

(defun rapid-package--expand-package (pkg-name condition p)
  "Return the expansion form for PKG-NAME from parsed plist P.
Wraps in (when CONDITION ...) unless CONDITION is t."
  (let* ((body (rapid-package--codegen-package
                pkg-name rapid-package--expanders p)))
    (if (eq condition t) body `(when ,condition ,body))))

(defmacro rapid-package-conf (category &rest args)
  "Configure Emacs settings by CATEGORY.

CATEGORY can be followed by an optional docstring.

Supported keywords
  :variable PAIRS       - Set variables (supports 3-element docstring)
                          Values support ,EXPR for runtime
                          evaluation
  :variable-default     - Set default values (supports 3-element
                          docstring)
                          Values support ,EXPR for runtime
                          evaluation
  :custom PAIRS         - Customize variables; values support ,EXPR for
                          runtime evaluation
  :hook PAIRS           - Add hooks
  :bind PAIRS           - Global key bindings (supports :map KEYMAP ...)
  :bind-keymap PAIRS    - Bind prefix keys to keymaps
  :unbind KEYS          - Unbind keys (string or :map format)
  :mode-enable MODES    - Enable modes
  :mode-disable MODES   - Disable modes
  :after FEATURES       - Apply body after these features are loaded
  :with MODE [ID] SUBS  - Mode-local setup (shorthand); or
                          :with (BLOCKS...)
                          SUBS: (:local VAR VAL ...) (:hook FN ...)
                                (:bind KEY CMD ...) (:unbind KEY ...)
                          :local supports 3-element docstring and
                          ,EXPR values
                          :hook FN calls (FN) in the generated hook
                          function
                          Generates a named hook function +
                          add-hook
  :init BODY...         - Execute at init
  :config BODY...       - Execute at config
  :when CONDITION       - Conditional execution
  :unless CONDITION     - Conditional execution (negated)
  :when-system SYSTEMS  - Execute only on specified system types (OR condition)
  :unless-system SYSTEMS - Execute except on specified system types (NAND)
  :when-gui BOOL        - Execute only in GUI environment
  :when-tty BOOL        - Execute only in TTY environment
  :when-ge PAIRS        - Execute when >= comparisons hold: ((VAR VAL) ...)
  :when-gt PAIRS        - Execute when > comparisons hold
  :when-le PAIRS        - Execute when <= comparisons hold
  :when-lt PAIRS        - Execute when < comparisons hold
  :when-eq PAIRS        - Execute when = comparisons hold
  :when-ne PAIRS        - Execute when /= comparisons hold
  :when-p PREDICATES    - Execute when predicates return non-nil
  :disabled BOOL        - Disable this block entirely (skip all code generation)

Example:
  (rapid-package-conf ui
    \"User interface settings\"
    :variable
    (ring-bell-function \\='ignore \"Disable bell\")
    (fill-column 80 \"Line width limit\")
    :bind
    (:map key-translation-map
          (\"C-h\" . \"<DEL>\"))
    :mode-enable (column-number-mode show-paren-mode)
    :mode-disable (menu-bar-mode tool-bar-mode))"
  (declare (indent defun))
  (let* ((p (condition-case err
                (rapid-package-dsl-parse (cons category args) rapid-package-conf-schema)
              (error
               (if rapid-package--loading-file
                   (rapid-package--abort 'dsl "%s:%d: %s: %s"
                                         rapid-package--loading-file
                                         rapid-package--loading-line
                                         category
                                         (error-message-string err))
                 (rapid-package--abort 'dsl "%s: %s"
                                       category
                                       (error-message-string err))))))
         (head-parsed (rapid-package--parse-head (plist-get p :_head)))
         (cat      (car head-parsed))
         (condition (rapid-package--check-condition p)))

    ;; Apply registered rewriters
    (setq p (rapid-package--apply-rewriters p cat 'conf))

    (rapid-package--expand-conf cat condition p)))

(defun rapid-package--expand-conf (cat condition p)
  "Return the expansion form for conf category CAT from parsed plist P.
Wraps in (when CONDITION ...) unless CONDITION is t."
  (let* ((body (rapid-package--codegen-conf cat rapid-package--expanders p)))
    (if (eq condition t) body `(when ,condition ,body))))

;;; rapid-package-after macro

(defun rapid-package-after--build-category-suffix (args)
  "Build category name suffix from conditional keywords in ARGS.
Returns a string to append to \\='after-PACKAGE\\=', or empty string
if no conditions.
Ignores :when and :unless (too complex for naming)."
  (let ((parts '())
        (tail args))
    ;; Skip leading non-keyword elements (e.g. optional docstring)
    (while (and tail (not (keywordp (car tail))))
      (setq tail (cdr tail)))
    ;; Parse args to extract conditional keywords
    (while tail
      (let ((key (car tail))
            (val (cadr tail)))
        (pcase key
          ;; :when-system
          (:when-system
           (push "when-system" parts)
           (let ((systems (cond
                           ;; List of systems: (windows-nt darwin)
                           ((listp val) val)
                           ;; Single symbol: windows-nt
                           ((symbolp val) (list val))
                           ;; Shouldn't happen, but handle it
                           (t (list val)))))
             (dolist (sys systems)
               ;; Handle both quoted and unquoted symbols
               (let ((sys-name (if (and (listp sys) (eq (car sys) 'quote))
                                   (cadr sys)
                                 sys)))
                 (push (symbol-name sys-name) parts)))))
          
          ;; :unless-system
          (:unless-system
           (push "unless-system" parts)
           (let ((systems (cond
                           ;; List of systems
                           ((listp val) val)
                           ;; Single symbol
                           ((symbolp val) (list val))
                           (t (list val)))))
             (dolist (sys systems)
               (let ((sys-name (if (and (listp sys) (eq (car sys) 'quote))
                                   (cadr sys)
                                 sys)))
                 (push (symbol-name sys-name) parts)))))
          
          ;; :when-gui
          (:when-gui
           (when val
             (push "when-gui" parts)))
          
          ;; :when-tty
          (:when-tty
           (when val
             (push "when-tty" parts)))
          
          ;; :when-ge
          (:when-ge
           (push "when-ge" parts)
           (let ((pairs (cond
                         ((and (listp val) (listp (car val))) val)
                         ((listp val) (list val))
                         (t (list val)))))
             (dolist (pair pairs)
               (let ((var (if (consp pair) (car pair) pair))
                     (value (if (consp pair) (cadr pair) nil)))
                 (push (format "%s" var) parts)
                 (when value
                   (push (format "%s" value) parts))))))
          
          ;; :when-gt
          (:when-gt
           (push "when-gt" parts)
           (let ((pairs (cond
                         ((and (listp val) (listp (car val))) val)
                         ((listp val) (list val))
                         (t (list val)))))
             (dolist (pair pairs)
               (let ((var (if (consp pair) (car pair) pair))
                     (value (if (consp pair) (cadr pair) nil)))
                 (push (format "%s" var) parts)
                 (when value
                   (push (format "%s" value) parts))))))
          
          ;; :when-le
          (:when-le
           (push "when-le" parts)
           (let ((pairs (cond
                         ((and (listp val) (listp (car val))) val)
                         ((listp val) (list val))
                         (t (list val)))))
             (dolist (pair pairs)
               (let ((var (if (consp pair) (car pair) pair))
                     (value (if (consp pair) (cadr pair) nil)))
                 (push (format "%s" var) parts)
                 (when value
                   (push (format "%s" value) parts))))))
          
          ;; :when-lt
          (:when-lt
           (push "when-lt" parts)
           (let ((pairs (cond
                         ((and (listp val) (listp (car val))) val)
                         ((listp val) (list val))
                         (t (list val)))))
             (dolist (pair pairs)
               (let ((var (if (consp pair) (car pair) pair))
                     (value (if (consp pair) (cadr pair) nil)))
                 (push (format "%s" var) parts)
                 (when value
                   (push (format "%s" value) parts))))))
          
          ;; :when-eq
          (:when-eq
           (push "when-eq" parts)
           (let ((pairs (cond
                         ((and (listp val) (listp (car val))) val)
                         ((listp val) (list val))
                         (t (list val)))))
             (dolist (pair pairs)
               (let ((var (if (consp pair) (car pair) pair))
                     (value (if (consp pair) (cadr pair) nil)))
                 (push (format "%s" var) parts)
                 (when value
                   (push (format "%s" value) parts))))))
          
          ;; :when-ne
          (:when-ne
           (push "when-ne" parts)
           (let ((pairs (cond
                         ((and (listp val) (listp (car val))) val)
                         ((listp val) (list val))
                         (t (list val)))))
             (dolist (pair pairs)
               (let ((var (if (consp pair) (car pair) pair))
                     (value (if (consp pair) (cadr pair) nil)))
                 (push (format "%s" var) parts)
                 (when value
                   (push (format "%s" value) parts))))))
          
          ;; :when-p
          (:when-p
           (push "when-p" parts)
           (let ((preds (cond
                         ((listp val) val)
                         ((symbolp val) (list val))
                         (t (list val)))))
             (dolist (pred preds)
               (push (symbol-name pred) parts)))))
        
        (setq tail (cddr tail))))
    
    (if parts
        (concat "-" (string-join (nreverse parts) "-"))
      "")))

;;;###autoload
(defmacro rapid-package-after (package &rest args)
  "Configure settings to apply after PACKAGE is loaded.

This is a convenience macro that expands to `rapid-package-conf'
with :after PACKAGE.
The category name is automatically generated as:
  after-PACKAGE[-CONDITION-SUFFIX]

CONDITION-SUFFIX is built from conditional keywords like :when-system,
:when-gui, etc.
Complex conditions (:when/:unless with arbitrary expressions) are
ignored for naming.

Example:
  (rapid-package-after magit
    :config
    (setq magit-diff-refine-hunk \\='all))

  ;; With conditions
  (rapid-package-after magit
    :when-system (darwin)
    :when-gui t
    :config
    (setq magit-use-gui-features t))

Expands to:
  (rapid-package-conf after-magit-when-system-darwin-when-gui
    :after magit
    :when-system (darwin)
    :when-gui t
    :config
    (setq magit-use-gui-features t))"
  (declare (indent defun))
  (let* ((pkg-name (if (symbolp package) package
                     (rapid-package--abort 'api
                                           "package argument must be a symbol, got: %S"
                                           package)))
         (suffix (rapid-package-after--build-category-suffix args))
         (category (intern (concat "after-" (symbol-name pkg-name) suffix))))
    `(rapid-package-conf ,category
       ,@args
       :after ,pkg-name)))

;;; File Loading

;;;###autoload
(defun rapid-package-load (file)
  "Load FILE containing rapid-package forms.

FILE can be a .el file or a .json file.

.el files are macro-expanded, byte-compiled to .elc for fast subsequent
loads, and optionally written to a sibling .json file (see
`rapid-package-json-auto-write').  The .json represents the same
configuration in a portable, human-readable form and can be passed
directly to this function.

.json files are processed item-by-item: each item is decoded, parsed,
expanded, and eval'd before moving to the next.  The resulting forms
are compiled to .elc so subsequent loads skip this work.  Supported
item types:
  \"package\"  - rapid-package definition
  \"config\"   - rapid-package-conf definition

Cache validation uses timestamps and optionally content hashes
(controlled by `rapid-package-use-hash-validation')."
  (interactive "fLoad rapid-package file: ")
  (let* ((source-file (expand-file-name file))
         (source-type (rapid-package--file-type source-file)))
    
    (unless (file-exists-p source-file)
      (rapid-package--abort 'load "File not found: %s" source-file))
    
    (pcase source-type
      ('elisp (rapid-package--load-elisp source-file))
      ('json (rapid-package--load-json source-file))
      (_ (rapid-package--abort 'load "Unsupported file type: %s (expected .el or .json)" source-file)))))

(defun rapid-package--file-type (file)
  "Return file type of FILE: \\='elisp or \\='json or nil."
  (let ((ext (downcase (or (file-name-extension file) ""))))
    (cond
     ((string= ext "el") 'elisp)
     ((string= ext "json") 'json)
     (t nil))))

(defun rapid-package--json-path-for (source-file)
  "Return the sibling .json path for SOURCE-FILE.
For example, /foo/bar/packages.el -> /foo/bar/packages.json."
  (concat (file-name-sans-extension source-file) ".json"))

(defun rapid-package--load-elisp (source-file)
  "Load Elisp SOURCE-FILE with compilation caching."
  (let* ((cache-files (rapid-package--cache-paths source-file))
         (cache-elc  (plist-get cache-files :elc))
         (cache-meta (plist-get cache-files :meta)))

    (unless (file-exists-p rapid-package-cache-dir)
      (make-directory rapid-package-cache-dir t))

    (if (rapid-package--cache-valid-p source-file cache-files)
        (progn
          (rapid-package--message 'cache "Loading compiled: %s"
                                  (file-name-nondirectory cache-elc))
          (load cache-elc nil t)
          (when (eq rapid-package-json-auto-write 'always)
            (let ((packages-data (rapid-package--extract-data source-file))
                  (json-file (rapid-package--json-path-for source-file)))
              (rapid-package--export-json packages-data json-file))))

      (rapid-package--message 'cache "Compiling: %s"
                              (file-name-nondirectory source-file))
      (let* ((expanded-forms (rapid-package--expand-file source-file))
             (json-file (rapid-package--json-path-for source-file)))
        (unless (rapid-package--compile-to-cache expanded-forms cache-elc)
          (rapid-package--message 'cache
                                  "Warning: Failed to generate cache for %s (will recompile next time)"
                                  (file-name-nondirectory source-file)))
        (when rapid-package-json-auto-write
          (rapid-package--export-json (rapid-package--extract-data source-file) json-file))
        (rapid-package--save-meta source-file cache-meta)
        ;; Note: No load here - expand-file already evaluated all forms
        (rapid-package--message 'cache "Compiled: %s (already evaluated during expansion)"
                                (file-name-nondirectory source-file))))))

(defun rapid-package--load-json (source-file)
  "Load JSON SOURCE-FILE, processing each item sequentially.

Each item is decoded, parsed, expanded, and eval'd in order before
the next item is processed.  This ensures keyword and expander
registrations in one item are visible to all subsequent items.

On first load the expanded forms are compiled to .elc so subsequent
loads skip this work."
  (let* ((cache-files (rapid-package--cache-paths source-file))
         (cache-elc  (plist-get cache-files :elc))
         (cache-meta (plist-get cache-files :meta)))

    (unless (file-exists-p rapid-package-cache-dir)
      (make-directory rapid-package-cache-dir t))

    (if (rapid-package--cache-valid-p source-file cache-files)
        (progn
          (rapid-package--message 'cache "Loading cached: %s"
                                  (file-name-nondirectory cache-elc))
          (load cache-elc nil t))

      (rapid-package--message 'json "Importing JSON: %s"
                              (file-name-nondirectory source-file))
      (let* ((result     (rapid-package--read-json source-file))
             (json-data  (car result))
             (item-lines (cdr result))
             (forms      (rapid-package--process-items json-data source-file item-lines)))
        ;; forms already eval'd item-by-item inside process-items;
        ;; compile them to cache so the next load can skip this work.
        (unless (rapid-package--compile-to-cache forms cache-elc)
          (rapid-package--message 'cache
                                  "Warning: Failed to generate cache for %s (will recompile next time)"
                                  (file-name-nondirectory source-file)))
        (rapid-package--save-meta source-file cache-meta)
        (rapid-package--message 'json "Imported and loaded: %s"
                                (file-name-nondirectory source-file))))))

(defun rapid-package--process-items (json-data file item-lines)
  "Process the items array from JSON-DATA one item at a time.

FILE is the source JSON file path used for error messages.  ITEM-LINES is a
vector of line numbers for each item, as returned by `rapid-package--read-json'.

Each item is fully processed in sequence:
  1. JSON decode (already done - item is a hash table)
  2. DSL parse   (json -> plist)
  3. Macro expand (plist -> elisp form)
  4. eval        (run the form now)

The eval in step 4 makes any keyword/expander registrations in that
item's :config immediately available to the next item's DSL parse and
macro expansion.  Execution order matches the JSON item order exactly.

Returns the list of expanded forms (for writing to .elc cache)."
  (let ((items (gethash "items" json-data))
        (forms (rapid-package--tl-new)))

    (unless items
      (rapid-package--abort 'json "JSON missing required 'items' field"))

    (dotimes (i (length items))
      (let* ((item (aref items i))
             (type (gethash "type" item))
             (rapid-package--loading-file file)
             (rapid-package--loading-line (aref item-lines i)))
        (pcase type
          ("package"
           (let* ((plist     (rapid-package--json-to-parsed item rapid-package-schema))
                  (head      (plist-get plist :_head))
                  (form      (rapid-package--expand-package
                              (car head)
                              (rapid-package--check-condition plist)
                              plist)))
             (eval form t)
             (rapid-package--tl-append! forms form)))

          ("config"
           (let* ((plist     (rapid-package--json-to-parsed item rapid-package-conf-schema))
                  (head      (plist-get plist :_head))
                  (form      (rapid-package--expand-conf
                              (car head)
                              (rapid-package--check-condition plist)
                              plist)))
             (eval form t)
             (rapid-package--tl-append! forms form)))

          ("fontset"
           (let* ((data (rapid-package-fontset--from-json item))
                  (form (rapid-package-fontset--expand-from-data data)))
             (when form
               (eval form t)
               (rapid-package--tl-append! forms form))))

          (_ (rapid-package--warning 'json
                                     "%sunknown item type: %s (expected \"package\", \"config\", or \"fontset\")"
                                     (rapid-package--loc) type)))))

    (rapid-package--tl-value forms)))

;;; Cache Management

(defun rapid-package--cache-paths (source-file)
  "Return cache file paths for SOURCE-FILE as a plist.

Cache file names encode both the base name and an MD5 of the absolute
path, preventing collisions between same-named files in different
directories.  For example, configs/ui.el and private/ui.el produce
distinct cache entries.

Returns: (:elc PATH :meta PATH)."
  (let* ((abs       (expand-file-name source-file))
         (base-name (file-name-sans-extension (file-name-nondirectory abs)))
         (path-hash (substring (md5 abs) 0 8))
         (cache-base (expand-file-name
                      (concat base-name "--" path-hash)
                      rapid-package-cache-dir)))
    (let ((compiled-ext (if (and rapid-package-native-compile
                                 (featurep 'native-compile))
                            ".eln"
                          ".elc")))
      (list :elc  (concat cache-base compiled-ext)
            :meta (concat cache-base ".meta")))))

(defun rapid-package--cache-valid-p (source-file cache-paths)
  "Return non-nil if the cache for SOURCE-FILE is ready to use.

Checks that:
  1. The .meta file exists and source content matches (timestamp or hash).
  2. The .elc file exists (so loading will succeed).
  3. Emacs version matches (byte-code compatibility).
  4. rapid-package version matches (schema/behavior compatibility).

CACHE-PATHS is the plist returned by `rapid-package--cache-paths'."
  (let ((meta-file (plist-get cache-paths :meta))
        (elc-file  (plist-get cache-paths :elc)))
    (and (file-exists-p meta-file)
         (file-exists-p elc-file)
         (let ((meta (rapid-package--read-meta meta-file)))
           (and meta
                ;; Check Emacs version compatibility
                (string= (plist-get meta :emacs-version) emacs-version)
                ;; Check rapid-package version compatibility
                (string= (plist-get meta :rapid-package-version) rapid-package-version)
                ;; Check cache type compatibility (byte vs native)
                (eq (plist-get meta :cache-type)
                    (if (rapid-package--native-compile-p) 'native 'byte))
                ;; Check source file validity
                (if rapid-package-use-hash-validation
                    (rapid-package--cache-valid-hybrid-p source-file meta)
                  (rapid-package--cache-valid-timestamp-p source-file meta)))))))

(defun rapid-package--cache-valid-timestamp-p (source-file meta)
  "Check cache validity using only timestamp.

SOURCE-FILE is the source file path.
META is the metadata plist."
  (let ((source-mtime (file-attribute-modification-time
                       (file-attributes source-file)))
        (cached-mtime (plist-get meta :source-mtime)))
    (and cached-mtime
         (time-equal-p source-mtime cached-mtime))))

(defun rapid-package--cache-valid-hybrid-p (source-file meta)
  "Check cache validity using timestamp + hash.

Fast path: if timestamps match, cache is valid.
Slow path: if timestamps differ, check content hash.

SOURCE-FILE is the source file path.
META is the metadata plist."
  (let ((source-mtime (file-attribute-modification-time
                       (file-attributes source-file)))
        (cached-mtime (plist-get meta :source-mtime))
        (cached-hash (plist-get meta :source-hash)))
    (cond
     ;; Timestamps match -> valid (fast path)
     ((and cached-mtime (time-equal-p source-mtime cached-mtime))
      t)
     ;; Timestamps differ, check hash
     (cached-hash
      (let ((current-hash (rapid-package--file-hash source-file)))
        (string= current-hash cached-hash)))
     ;; No hash info
     (t nil))))

(defun rapid-package--file-hash (file)
  "Calculate SHA256 hash of FILE content."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun rapid-package--save-meta (source-file meta-file)
  "Save metadata about SOURCE-FILE to META-FILE."
  (let ((meta (list :source-file source-file
                    :source-mtime (file-attribute-modification-time
                                   (file-attributes source-file))
                    :source-hash (when rapid-package-use-hash-validation
                                   (rapid-package--file-hash source-file))
                    :compile-time (current-time)
                    :emacs-version emacs-version
                    :rapid-package-version rapid-package-version
                    :cache-type (if (rapid-package--native-compile-p) 'native 'byte))))
    (with-temp-file meta-file
      (let ((print-length nil)
            (print-level nil))
        (prin1 meta (current-buffer))))))

(defun rapid-package--read-meta (meta-file)
  "Read metadata from META-FILE.
Returns nil if the file doesn't exist or is corrupted."
  (when (file-exists-p meta-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents meta-file)
          (goto-char (point-min))
          (read (current-buffer)))
      (error
       (rapid-package--message 'cache "Corrupted meta file: %s (will recompile)"
                               (file-name-nondirectory meta-file))
       nil))))

;;;###autoload
(defun rapid-package-clear-cache (&optional file)
  "Clear compilation cache for FILE, or all caches if FILE is nil.

Only .elc and .meta files in `rapid-package-cache-dir' are removed.
The sibling .json file (if any) is not touched."
  (interactive)
  (if file
      (let ((cache-files (rapid-package--cache-paths file)))
        (dolist (cache-file (list (plist-get cache-files :elc)
                                  (plist-get cache-files :meta)))
          (when (file-exists-p cache-file)
            (delete-file cache-file)
            (rapid-package--message 'cache "Deleted %s" cache-file))))
    (when (file-exists-p rapid-package-cache-dir)
      (delete-directory rapid-package-cache-dir t)
      (rapid-package--message 'cache "Cleared all rapid-package cache"))))

;;;###autoload
(defun rapid-package-recompile (file)
  "Force recompile FILE even if cache is valid."
  (interactive "fRecompile file: ")
  (rapid-package-clear-cache file)
  (rapid-package-load file))

;;; Compilation

(defun rapid-package--native-compile-p ()
  "Return non-nil if native compilation is both requested and available."
  (and rapid-package-native-compile (featurep 'native-compile)))

(defun rapid-package--compile-to-elc-only (forms output-elc)
  "Byte-compile FORMS to OUTPUT-ELC via a temporary .el file.
Returns t if successful, nil if compilation failed."
  (let* ((temp-el  (make-temp-file "rapid-compile-" nil ".el"))
         (temp-elc (concat (file-name-sans-extension temp-el) ".elc"))
         (success nil))
    (unwind-protect
        (progn
          (with-temp-file temp-el
            (insert ";;; Compiled by rapid-package -*- lexical-binding: t; -*-\n\n")
            (dolist (form forms)
              (prin1 form (current-buffer))
              (insert "\n\n")))
          (let ((byte-compile-verbose nil)
                (byte-compile-warnings nil))
            (byte-compile-file temp-el))
          (if (file-exists-p temp-elc)
              (progn
                (copy-file temp-elc output-elc t)
                (setq success t))
            (rapid-package--message 'cache 
                                    "Warning: Cache generation failed for %s (byte-compile did not produce .elc)"
                                    (file-name-nondirectory output-elc))))
      (when (file-exists-p temp-el)  (delete-file temp-el))
      (when (file-exists-p temp-elc) (delete-file temp-elc)))
    success))

(defun rapid-package--compile-to-eln (forms output-eln)
  "Native-compile FORMS to OUTPUT-ELN via a temporary .el file.
Returns t if successful, nil if compilation failed."
  (let* ((temp-el (make-temp-file "rapid-compile-" nil ".el"))
         (success nil))
    (unwind-protect
        (progn
          (with-temp-file temp-el
            (insert ";;; Compiled by rapid-package -*- lexical-binding: t; -*-\n\n")
            (dolist (form forms)
              (prin1 form (current-buffer))
              (insert "\n\n")))
          (condition-case err
              (progn
                (native-compile temp-el output-eln)
                (if (file-exists-p output-eln)
                    (setq success t)
                  (rapid-package--message 'cache
                                          "Warning: Cache generation failed for %s (native-compile did not produce .eln)"
                                          (file-name-nondirectory output-eln))))
            (error
             (rapid-package--message 'cache
                                     "Warning: Native compilation error for %s: %s"
                                     (file-name-nondirectory output-eln) (error-message-string err)))))
      (when (file-exists-p temp-el) (delete-file temp-el)))
    success))

(defun rapid-package--compile-to-cache (forms output-file)
  "Compile FORMS to OUTPUT-FILE using native or byte compilation.
Uses native compilation when `rapid-package-native-compile' is non-nil and
native compilation is available; otherwise uses byte compilation.
Falls back to byte compilation with a warning if native compilation is
requested but unavailable.
Returns t if successful, nil if compilation failed."
  (cond
   ((rapid-package--native-compile-p)
    (rapid-package--compile-to-eln forms output-file))
   ((and rapid-package-native-compile (not (featurep 'native-compile)))
    (rapid-package--message 'cache
                            "Warning: Native compilation requested but not available; falling back to byte compilation")
    (rapid-package--compile-to-elc-only forms output-file))
   (t
    (rapid-package--compile-to-elc-only forms output-file))))

;;; Elisp File Processing

(defun rapid-package--expand-file (file)
  "Read FILE and expand all rapid-package macros.

Forms are evaluated immediately during expansion so that extension
registrations (add-keyword, add-expander, etc.) in one form are
available when parsing the next form in the same file.

Returns a list of expanded forms ready for compilation."
  (let ((forms (rapid-package--tl-new))
        (rapid-package--expansion-phase t))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case err
          (while (not (eobp))
            (let* ((form-start (point))
                   (form       (read (current-buffer)))
                   (rapid-package--loading-file file)
                   (rapid-package--loading-line (line-number-at-pos form-start))
                   (expanded   (rapid-package--maybe-expand form)))
              (condition-case eval-err
                  (eval expanded t)
                (error
                 (let* ((line    (line-number-at-pos form-start))
                        (excerpt (buffer-substring-no-properties
                                  form-start
                                  (min (+ form-start 60) (point-max)))))
                   (rapid-package--abort 'eval
                                         "%s:%d: %s\n  Near: %s"
                                         file line (error-message-string eval-err)
                                         (string-trim excerpt)))))
              (rapid-package--tl-append! forms expanded)))
        (end-of-file nil)
        (error
         (let* ((line    (line-number-at-pos))
                (excerpt (buffer-substring-no-properties
                          (line-beginning-position)
                          (min (+ (line-beginning-position) 60)
                               (point-max)))))
           (rapid-package--abort 'parse
                                 "%s:%d: %s\n  Near: %s"
                                 file line (error-message-string err)
                                 (string-trim excerpt))))))
    (rapid-package--tl-value forms)))

(defun rapid-package--maybe-expand (form)
  "Expand FORM if it's a rapid-package macro, otherwise return as-is."
  (if (and (listp form)
           (memq (car form) '(rapid-package rapid-package-conf rapid-package-after
                               rapid-package-fontset)))
      (macroexpand-all form)
    form))

(defun rapid-package--extract-data (file)
  "Extract package data from FILE for JSON export.

Returns a list of (:type TYPE :data PARSED-PLIST) entries."
  (let ((packages (rapid-package--tl-new)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while (not (eobp))
            (let ((form (read (current-buffer))))
              (cond
               ((and (listp form) (eq (car form) 'rapid-package))
                (rapid-package--tl-append!
                 packages
                 (list :type 'package
                       :data (rapid-package-dsl-parse (cdr form) rapid-package-schema))))
               ((and (listp form) (eq (car form) 'rapid-package-conf))
                (rapid-package--tl-append!
                 packages
                 (list :type 'config
                       :data (rapid-package-dsl-parse (cdr form) rapid-package-conf-schema))))
               ;; Handle rapid-package-after by expanding it to rapid-package-conf
               ((and (listp form) (eq (car form) 'rapid-package-after))
                (let ((expanded (macroexpand-1 form)))
                  (when (and (listp expanded) (eq (car expanded) 'rapid-package-conf))
                    (rapid-package--tl-append!
                     packages
                     (list :type 'config
                           :data (rapid-package-dsl-parse (cdr expanded) rapid-package-conf-schema))))))
               ((and (listp form) (eq (car form) 'rapid-package-fontset))
                (rapid-package--tl-append!
                 packages
                 (list :type 'fontset
                       :data (rapid-package-fontset--parse-args (cdr form))))))))
        (end-of-file nil)))
    (rapid-package--tl-value packages)))

;;; JSON Conversion Helpers

(defun rapid-package-json--get-field-type (key schema)
  "Get the type of field KEY from SCHEMA.
SCHEMA is a schema alist in the same format as `rapid-package-schema'.
Returns: body, ir, or lisp-value"
  (let ((schema-entry (assq key schema)))
    (if schema-entry
        (let ((spec (cdr schema-entry)))
          (cond
           ((eq spec 'body) 'body)
           ((eq spec 'alist) 'ir)
           ;; Standard schema types are lisp-value
           ((memq spec '(single list flag)) 'lisp-value)
           ;; Function or cons with function (parser/finalizer)
           ((or (functionp spec)
                (and (consp spec) (functionp (car spec))))
            'ir)
           (t 'lisp-value)))
      'lisp-value)))

(defun rapid-package-json--format-body-to-flat-array (forms)
  "Format FORMS (list of S-expressions) into flat array of code lines."
  (let ((all-lines (rapid-package--tl-new)))
    (dolist (form forms)
      (let* ((fill-column 80)
             (print-escape-newlines t)
             (pp-escape-newlines t)
             (print-length nil)
             (print-level nil)
             (indent-tabs-mode nil)
             (formatted (if (> (cdr (func-arity 'pp-to-string)) 1)
                            (with-suppressed-warnings ((callargs pp-to-string)
                                                       (unresolved pp-fill))
                              (pp-to-string form #'pp-fill))
                          (pp-to-string form)))
             (lines (split-string formatted "\n" t)))
        ;; split-string returns a fresh list, safe to pass ownership
        (rapid-package--tl-extend! all-lines lines)))
    (vconcat (rapid-package--tl-value all-lines))))

(defun rapid-package-json--parse-flat-array-to-body (lines-array)
  "Parse flat array of code lines back into list of forms."
  (let ((full-text (string-join (append lines-array nil) "\n")))
    (with-temp-buffer
      (insert full-text)
      (goto-char (point-min))
      (let ((forms '()))
        (condition-case err
            (while (not (eobp))
              (push (read (current-buffer)) forms))
          (error 
           (rapid-package--abort 'json
                                 "Failed to parse JSON body forms: %s\nContent: %s"
                                 (error-message-string err)
                                 (if (> (length full-text) 200)
                                     (concat (substring full-text 0 200) "...")
                                   full-text))))
        (nreverse forms)))))


(defun rapid-package-json--unquote (val)
  "Remove quote wrapper from VAL if present.
Handles (quote X) -> X."
  (if (and (consp val) (eq (car val) 'quote))
      (cadr val)
    val))

(defun rapid-package-json--normalize-lisp-value (value)
  "Normalize a Lisp VALUE to a JSON-serializable form with readable prefixes.

Mapping:
  t                -> JSON true
  nil              -> JSON false
  number           -> number
  string           -> string (escaped if starts with identifier prefix)
  keyword          -> \":keyword\" (: prefix)
  symbol           -> \"\\='symbol\" (\\=' prefix)
  (function X)     -> \"#\\='X\" (function quote)
  (\\, EXPR)       -> \",EXPR\" (unquote prefix)
  list/cons        -> \"(...)\" (prin1-to-string)

Strings starting with identifier prefixes (#, \\=', :, (, `, \", \\) are
escaped with a backslash to avoid ambiguity.

`rapid-package-json--denormalize-lisp-value' reverses this by
recognizing prefixes."
  (let ((v (rapid-package-json--unquote value)))
    (cond
     ;; Boolean
     ((eq v t)    t)
     ((eq v nil)  :json-false)
     
     ;; Number
     ((numberp v) v)
     
     ;; String - escape if starts with identifier prefix
     ((stringp v)
      (if (string-match-p "^[#':(`\\\"]" v)
          (concat "\\" v)
        v))
     
     ;; Unquote: (\, EXPR) -> ",EXPR"
     ((and (consp v) (eq (car v) '\,))
      (format ",%s" (prin1-to-string (cadr v))))

     ;; Function quote: #'func
     ((and (consp v) (eq (car v) 'function))
      (format "#'%s" (cadr v)))

     ;; Keyword: :foo
     ((keywordp v)
      (symbol-name v))
     
     ;; Symbol: 'foo
     ((symbolp v)
      (format "'%s" (symbol-name v)))
     
     ;; List/cons: (a b c) or ((a . b) ...)
     ((consp v)
      (prin1-to-string v))
     
     ;; Fallback
     (t (prin1-to-string v)))))

(defun rapid-package-json--denormalize-lisp-value (value)
  "Reverse `rapid-package-json--normalize-lisp-value'.

Recognizes prefixes and escapes:
  JSON true/false   -> t/nil
  number            -> number
  \"\\X\"            -> X (escaped string, backslash removed)
  \"#\\='X\"         -> (function X)
  \"\\='X\"          -> X (symbol)
  \":X\"             -> :X (keyword)
  \",EXPR\"          -> (\\, EXPR) (unquote)
  \"(...)\"          -> read (list/cons)
  \"[...]\"          -> read (vector)
  \"?X\"             -> read (character)
  plain string      -> string (as-is)"
  (cond
   ((eq value t)           t)
   ((eq value :json-false) nil)
   ((eq value :json-null)  nil)
   ((numberp value) value)
   ((stringp value)
    (cond
     ;; Escaped string: \X -> X (remove backslash)
     ((string-prefix-p "\\" value)
      (substring value 1))
     
     ;; Function quote: #'func
     ((string-prefix-p "#'" value)
      (list 'function (intern (substring value 2))))
     
     ;; Symbol: 'func
     ((string-prefix-p "'" value)
      (intern (substring value 1)))
     
     ;; Keyword: :foo
     ((string-prefix-p ":" value)
      (intern value))
     
     ;; Unquote: ",EXPR" -> (\, EXPR)
     ((string-prefix-p "," value)
      (list '\, (condition-case nil
                    (read (substring value 1))
                  (error (substring value 1)))))

     ;; S-expression: (...) or [...] or ?char
     ((string-match-p "^[(\[?]" value)
      (condition-case nil
          (read value)
        (error value)))
     
     ;; Plain string
     (t value)))
   (t value)))

(defun rapid-package-json--format-form (form)
  "Format FORM as an indented Lisp expression.

For short forms (< 80 chars), returns the form as-is.
For longer forms, uses `pp-emacs-lisp-code' (Emacs 29.1+) or `pp'
to format with proper line breaks and indentation."
  (let ((simple (prin1-to-string form)))
    (if (< (length simple) 80)
        simple
      ;; Long form - format with line breaks and indentation
      (let* ((fill-column 80)
             (print-escape-newlines t)
             (pp-escape-newlines t)
             (print-length nil)
             (print-level nil)
             (indent-tabs-mode nil))
        (if (> (cdr (func-arity 'pp-to-string)) 1)
            (with-suppressed-warnings ((callargs pp-to-string))
              (pp-to-string form #'pp-fill))
          (pp-to-string form))))))

(defun rapid-package-json--normalize-ir-value (value)
  "Normalize VALUE that is an IR plist list or body form list.

IR plist list  ((:key K :command C) ...)  -> array of hash tables.
Body form list ((add-to-list ...) ...)    -> array of prin1 strings.
Anything else                             -> delegate to
  `rapid-package-json--normalize-lisp-value'."
  (cond
   ;; Unquote: (\, EXPR) -> delegate to normalize-lisp-value for ",EXPR" encoding
   ((and (consp value) (eq (car value) '\,))
    (rapid-package-json--normalize-lisp-value value))
   ((and (consp value)
         (consp (car value))
         (keywordp (caar value)))
    ;; IR plist list -> array of hash tables
    (let ((tl (rapid-package--tl-new)))
      (dolist (item value)
        (rapid-package--tl-append! tl (rapid-package-json--plist-to-hash item)))
      (vconcat (rapid-package--tl-value tl))))
   ((consp value)
    ;; List - convert to vector (check what kind of list it is)
    (let ((tl (rapid-package--tl-new)))
      (cond
       ;; List of symbols -> convert each and create vector
       ((and (cl-every #'symbolp value) (not (null value)))
        (dolist (item value)
          (rapid-package--tl-append! tl (rapid-package-json--normalize-lisp-value item))))
       ;; List of strings -> convert each and create vector
       ((and (cl-every #'stringp value) (not (null value)))
        (dolist (item value)
          (rapid-package--tl-append! tl (rapid-package-json--normalize-lisp-value item))))
       ;; Body forms -> format with indentation
       (t
        (dolist (item value)
          (rapid-package--tl-append! tl (rapid-package-json--format-form item)))))
      (vconcat (rapid-package--tl-value tl))))
   (t (rapid-package-json--normalize-lisp-value value))))

(defun rapid-package-json--plist-to-hash (plist)
  "Convert IR PLIST to a JSON hash table.
Keys are stored without the leading colon.
Values in :value and :default slots are converted with
`rapid-package-json--normalize-lisp-value'; all other slots use
`rapid-package-json--normalize-ir-value'."
  (let ((obj  (make-hash-table :test 'equal))
        (tail plist))
    (while tail
      (let* ((key     (car tail))
             (val     (cadr tail))
             (key-str (substring (symbol-name key) 1))
             (json-val (if (memq key '(:value :default))
                           (rapid-package-json--normalize-lisp-value val)
                         (rapid-package-json--normalize-ir-value val))))
        (puthash key-str json-val obj))
      (setq tail (cddr tail)))
    obj))

(defun rapid-package--export-metadata ()
  "Return a metadata hash table for JSON export (timestamp, Emacs version, etc.)."
  (let ((meta (make-hash-table :test 'equal)))
    (puthash "generated-at" (format-time-string "%Y-%m-%dT%H:%M:%S%z") meta)
    (puthash "emacs-version" emacs-version meta)
    (puthash "rapid-package-version" rapid-package-version meta)
    meta))

(defun rapid-package-json--field-json-value (key val schema)
  "Convert field KEY / VAL pair to a JSON-serializable value.
Uses schema-based type detection.
SCHEMA is a schema alist in the same format as `rapid-package-schema'."
  (let ((type (rapid-package-json--get-field-type key schema)))
    (cond
     ((eq type 'body)
      (if (consp val)
          (rapid-package-json--format-body-to-flat-array val)
        (rapid-package-json--normalize-lisp-value val)))
     ((eq type 'lisp-value)
      (if (consp val)
          (let ((tl (rapid-package--tl-new)))
            (dolist (item val)
              (rapid-package--tl-append! tl (rapid-package-json--normalize-lisp-value item)))
            (vconcat (rapid-package--tl-value tl)))
        (rapid-package-json--normalize-lisp-value val)))
     ((eq type 'ir)
      (if (consp val)
          (let ((tl (rapid-package--tl-new)))
            (dolist (item val)
              (rapid-package--tl-append! tl (rapid-package-json--plist-to-hash item)))
            (vconcat (rapid-package--tl-value tl)))
        (rapid-package-json--normalize-lisp-value val)))
     (t (rapid-package-json--normalize-lisp-value val)))))

(defun rapid-package-json--is-flag-p (key schema)
  "Return non-nil if KEY is a flag-type field in SCHEMA.
SCHEMA is a schema alist in the same format as `rapid-package-schema'."
  (let ((schema-entry (assq key schema)))
    (and schema-entry
         (eq (cdr schema-entry) 'flag))))

(defun rapid-package-json--flag-aware-json-value (key val schema)
  "Convert VAL to JSON value, preserving flag-type nil.
For flag-type fields, nil is converted to :json-false to preserve
information during JSON round-trip. For other fields, uses standard
conversion via `rapid-package-json--field-json-value'.
SCHEMA is a schema alist in the same format as `rapid-package-schema'."
  (if (rapid-package-json--is-flag-p key schema)
      (if val t :json-false)
    (rapid-package-json--field-json-value key val schema)))

(defun rapid-package--plist-to-json-generic (plist head-key json-obj schema)
  "Serialize PLIST into JSON-OBJ using HEAD-KEY for the name field.
SCHEMA is a schema alist in the same format as `rapid-package-schema'."
  (let* ((head      (plist-get plist :_head))
         (name      (car head))
         (docstring (cadr head)))
    (puthash head-key (symbol-name name) json-obj)
    (when docstring
      (puthash "description" docstring json-obj))
    (let ((tail plist))
      (while tail
        (let* ((key (car tail))
               (val (cadr tail)))
          ;; Export field if: (1) val is non-nil, OR (2) val is nil but key is flag-type
          (when (and (not (eq key :_head))
                     (or val (rapid-package-json--is-flag-p key schema)))
            (puthash (substring (symbol-name key) 1)
                     (rapid-package-json--flag-aware-json-value key val schema)
                     json-obj)))
        (setq tail (cddr tail))))))

(defun rapid-package--package-to-json (plist json-obj)
  "Convert package PLIST to JSON-OBJ using generic conversion."
  (rapid-package--plist-to-json-generic plist "name" json-obj rapid-package-schema))

(defun rapid-package--config-to-json (plist json-obj)
  "Convert config PLIST to JSON-OBJ using generic conversion."
  (rapid-package--plist-to-json-generic plist "name" json-obj rapid-package-conf-schema))

(defun rapid-package--to-json (plist type)
  "Convert PLIST to JSON object.

TYPE should be \\='package, \\='config, or \\='fontset.

Returns a hash table ready for JSON encoding."
  (let ((json-obj (make-hash-table :test 'equal)))
    (puthash "type" (symbol-name type) json-obj)
    (cond
     ((eq type 'fontset) (rapid-package-fontset--fill-json plist json-obj))
     ((eq type 'package) (rapid-package--package-to-json  plist json-obj))
     (t                  (rapid-package--config-to-json   plist json-obj)))
    json-obj))

(defun rapid-package--export-json (packages-data json-file)
  "Export PACKAGES-DATA to JSON-FILE in items format.

PACKAGES-DATA is a list of (:type TYPE :data PARSED-PLIST) entries."
  (let ((json-data (make-hash-table :test 'equal))
        (items (rapid-package--tl-new)))
    (dolist (item packages-data)
      (let ((type (plist-get item :type))
            (data (plist-get item :data)))
        (rapid-package--tl-append! items (rapid-package--to-json data type))))
    (puthash "items" (vconcat (rapid-package--tl-value items)) json-data)
    (puthash "metadata" (rapid-package--export-metadata) json-data)
    (with-temp-file json-file
      (let ((json-encoding-pretty-print t))
        (insert (json-encode json-data))))))

(defun rapid-package-json--denormalize-field (key json-val schema)
  "Reverse field-json-value for field KEY.
Uses schema-based type detection.
SCHEMA is a schema alist in the same format as `rapid-package-schema'."
  (let ((type (rapid-package-json--get-field-type key schema)))
    (cond
     ((eq type 'body)
      (cond
       ((vectorp json-val)
        (rapid-package-json--parse-flat-array-to-body json-val))
       ((eq json-val :json-false) nil)
       ((eq json-val :json-null) nil)
       (t (rapid-package-json--denormalize-lisp-value json-val))))
     ((eq type 'lisp-value)
      (cond
       ((vectorp json-val)
        (mapcar #'rapid-package-json--denormalize-lisp-value
                (append json-val nil)))
       ((eq json-val :json-false) nil)
       ((eq json-val :json-null) nil)
       (t (rapid-package-json--denormalize-lisp-value json-val))))
     ((eq type 'ir)
      (cond
       ((vectorp json-val)
        (mapcar #'rapid-package-json--hash-to-plist
                (append json-val nil)))
       (t (rapid-package-json--denormalize-lisp-value json-val))))
     (t (rapid-package-json--denormalize-lisp-value json-val)))))

(defun rapid-package-json--denormalize-ir-slot (val)
  "Denormalize an IR-level slot value VAL (not a Lisp value slot).

IR slots hold strings (keys), symbols (commands), or nested structures.
For strings without special prefixes, interns them as symbols.

  hash table         -> plist (nested IR)
  vector of hashes   -> list of plists
  vector of strings  -> list of symbols
  boolean            -> t/nil
  string with prefix -> denormalize-lisp-value (handles #', ', :, etc)
  plain string       -> intern as symbol
  anything else      -> as-is"
  (cond
   ((eq val t)           t)
   ((eq val :json-false) nil)
   ((eq val :json-null)  nil)
   ((hash-table-p val)
    (rapid-package-json--hash-to-plist val))
   ((and (vectorp val)
         (> (length val) 0)
         (hash-table-p (aref val 0)))
    (mapcar #'rapid-package-json--hash-to-plist (append val nil)))
   ((vectorp val)
    ;; Vector of strings -> list (handle each via denormalize logic)
    (mapcar (lambda (v)
              (if (stringp v)
                  (rapid-package-json--denormalize-ir-slot v)
                v))
            (append val nil)))
   ((stringp val)
    (cond
     ;; Escaped or has identifier prefix -> use denormalize-lisp-value
     ((string-match-p "^[\\#':(\[?,]" val)
      (rapid-package-json--denormalize-lisp-value val))
     ;; Plain string -> keep as string
     (t val)))
   (t val)))

(defun rapid-package-json--hash-to-plist (hash)
  "Convert JSON HASH table to an IR plist with keyword keys.

Values in :value and :default slots use denormalize-lisp-value and are
then conditionally re-quoted to match DSL parser output:
  - Self-evaluating values (keywords, numbers, strings, booleans) are not quoted
  - (function ...) forms are passed through as-is (no quote)
  - (quote ...) forms are passed through as-is (already quoted)
  - Everything else (symbols, lists) is wrapped in (quote ...)
All other slots use denormalize-ir-slot."
  (let ((plist nil))
    (maphash
     (lambda (key-str val)
       (let* ((kw  (intern (concat ":" key-str)))
              (out (if (memq kw '(:value :default))
                       (let ((v (rapid-package-json--denormalize-lisp-value val)))
                         ;; Re-quote to match DSL output, but handle special cases
                         (cond
                          ;; Self-evaluating values: no quote
                          ((or (keywordp v) (numberp v) (stringp v) 
                               (booleanp v) (eq v nil))
                           v)
                          ;; (function ...) forms: pass through
                          ((and (consp v) (eq (car v) 'function))
                           v)
                          ;; (quote ...) forms: already quoted, pass through
                          ((and (consp v) (eq (car v) 'quote))
                           v)
                          ;; Unquote forms: pass through as-is
                          ((and (consp v) (eq (car v) '\,))
                           v)
                          ;; Everything else: wrap in quote
                          (t (list 'quote v))))
                     (rapid-package-json--denormalize-ir-slot val))))
         (setq plist (plist-put plist kw out))))
     hash)
    plist))

(defun rapid-package--json-to-parsed (json-obj schema)
  "Convert JSON-OBJ to a parsed plist.
Returns a plist in the same format as `rapid-package-dsl-parse'.
SCHEMA is a schema alist in the same format as `rapid-package-schema'."
  (let ((plist nil))
    (maphash
     (lambda (key-str json-val)
       (let* ((kw (intern (concat ":" key-str))))
         (unless (memq kw '(:_head :name :description :type))
           (let ((out (rapid-package-json--denormalize-field kw json-val schema)))
             ;; Import field if: (1) out is non-nil, OR (2) out is nil but key is flag-type
             (when (or out (and (null out) (rapid-package-json--is-flag-p kw schema)))
               (setq plist (plist-put plist kw out)))))))
     json-obj)
    (let* ((head-key "name")
           (name-str (gethash head-key json-obj))
           (desc     (gethash "description" json-obj)))
      (unless (stringp name-str)
        (rapid-package--abort 'json "%smissing or invalid '%s' field (expected string, got %S)"
                              (rapid-package--loc) head-key name-str))
      (setq plist (plist-put plist :_head
                             (if desc (list (intern name-str) desc)
                               (list (intern name-str))))))
    plist))

(defun rapid-package--read-json (file)
  "Read and parse JSON FILE.

Returns (DATA . ITEM-LINES) where DATA is a hash table with the parsed JSON
data, and ITEM-LINES is a vector of line numbers for each item in the items
array.  Line numbers are best-effort: minified JSON will report line 1 for
all items."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((json-object-type 'hash-table)
          (json-array-type 'vector)
          (json-key-type 'string)
          (json-false :json-false)
          (json-null :json-null))
      (let* ((data  (json-read))
             (items (gethash "items" data))
             (n     (if items (length items) 0))
             (lines (make-vector n 0)))
        (when (> n 0)
          (goto-char (point-min))
          (when (re-search-forward "\"items\"[ \t\n\r]*:[ \t\n\r]*\\[" nil t)
            (skip-chars-forward " \t\n\r")
            (let ((i 0))
              (while (and (< i n) (not (looking-at "\\]")))
                (aset lines i (line-number-at-pos (point)))
                (json-read)
                (setq i (1+ i))
                (skip-chars-forward " \t\n\r,")))))
        (cons data lines)))))

;;; User Commands (Cache Management)

;;;###autoload
(defun rapid-package-cache-status ()
  "Show cache status in a formatted buffer."
  (interactive)
  (let ((buf (get-buffer-create "*rapid-package-cache*"))
        (files (when (file-exists-p rapid-package-cache-dir)
                 (directory-files rapid-package-cache-dir nil "\\.meta$"))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "# Rapid Package Cache Status\n\n")
        (insert (format "Cache directory: %s\n" rapid-package-cache-dir))
        (insert (format "Hash validation: %s\n\n"
                        (if rapid-package-use-hash-validation "enabled" "disabled")))

        (if (null files)
            (insert "No cached files.\n")
          (insert (format "Cached files: %d\n\n" (length files)))

          (dolist (meta-file files)
            (let* ((meta-path    (expand-file-name meta-file rapid-package-cache-dir))
                   (meta         (rapid-package--read-meta meta-path))
                   (source       (plist-get meta :source-file))
                   (compile-time (plist-get meta :compile-time))
                   (base         (file-name-sans-extension meta-file))
                   (elc-file     (expand-file-name (concat base ".elc") rapid-package-cache-dir))
                   (json-sibling (rapid-package--json-path-for source)))
              (insert (format "## %s\n\n" (file-name-nondirectory source)))
              (insert (format "- **Source:** %s\n" source))
              (insert (format "- **Compiled:** %s\n"
                              (format-time-string "%Y-%m-%d %H:%M:%S" compile-time)))
              (insert (format "- **Cache valid:** %s\n"
                              (if (and (file-exists-p source)
                                       (rapid-package--cache-valid-p
                                        source
                                        (list :elc elc-file :meta meta-path)))
                                  "Yes"
                                "No (source modified or elc missing)")))
              (insert (format "- **ELC:** %s (%s)\n"
                              (file-name-nondirectory elc-file)
                              (if (file-exists-p elc-file)
                                  (file-size-human-readable
                                   (file-attribute-size (file-attributes elc-file)))
                                "missing")))
              (insert (format "- **JSON:** %s\n"
                              (if (file-exists-p json-sibling)
                                  json-sibling
                                "not written")))
              (insert "\n"))))

        (goto-char (point-min))
        (when (fboundp 'markdown-mode)
          (markdown-mode))
        (view-mode 1)))
    (display-buffer buf)))

;;;###autoload
(defun rapid-package-clean-invalid-cache ()
  "Remove invalid .elc and .meta files from the cache directory.

Scans all cached entries and removes those whose source files have been
deleted or modified.  Sibling .json files are not touched."
  (interactive)
  (when (not (file-exists-p rapid-package-cache-dir))
    (rapid-package--abort 'cache "No cache directory exists"))

  (let ((files (directory-files rapid-package-cache-dir nil "\\.meta$"))
        (removed 0))
    (dolist (meta-file files)
      (let* ((meta-path (expand-file-name meta-file rapid-package-cache-dir))
             (base      (file-name-sans-extension meta-file))
             (meta      (rapid-package--read-meta meta-path))
             (source    (plist-get meta :source-file)))
        (when (or (not (file-exists-p source))
                  (not (rapid-package--cache-valid-p
                        source
                        (list :elc  (expand-file-name (concat base ".elc") rapid-package-cache-dir)
                              :meta meta-path))))
          (dolist (ext '(".elc" ".meta"))
            (let ((file (expand-file-name (concat base ext) rapid-package-cache-dir)))
              (when (file-exists-p file)
                (delete-file file)
                (setq removed (1+ removed)))))
          (rapid-package--message 'cache "Removed invalid cache for: %s" source))))
    (rapid-package--message 'cache "Cleaned %d cache file(s)" removed)))

(provide 'rapid-package)

;;; rapid-package.el ends here
