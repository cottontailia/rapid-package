;;; rapid-package-fontset.el --- Fontset configuration for rapid-package -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cottontailia

;; Author: Cottontailia
;; Additional-Author: AI Assistant
;; URL: https://github.com/cottontailia/rapid-package
;; License: CC0

;;; Commentary:

;; Fontset DSL, code generator, and JSON serialization for
;; `rapid-package-fontset'.
;;
;; This file is self-contained: it holds the DSL parser, the code
;; generator, the macro itself, and all JSON encode/decode logic for
;; the fontset item type.
;;
;; Functions used from rapid-package.el (declared below, not required):
;;   rapid-package--abort, rapid-package--parse-head,
;;   rapid-package--check-condition,
;;   rapid-package--plist-to-json-generic, rapid-package--json-to-parsed
;;
;; Function used from rapid-package-codegen.el (declared below):
;;   rapid-package--codegen-unquote

;;; Code:

(require 'rapid-package-tl)
(require 'rapid-package-dsl)

(declare-function rapid-package--abort "rapid-package")
(declare-function rapid-package--parse-head "rapid-package")
(declare-function rapid-package--check-condition "rapid-package")
(declare-function rapid-package--plist-to-json-generic "rapid-package")
(declare-function rapid-package--json-to-parsed "rapid-package")
(declare-function rapid-package--codegen-unquote "rapid-package-codegen")

;; Variables defined in rapid-package.el, used at runtime by parsers.
(defvar rapid-package--loading-file)
(defvar rapid-package--loading-line)

;;; Fontset DSL Parser

(defun rapid-package-dsl-parse-fontset-rules (item args _current-key current-acc)
  "Custom DSL parser for :rules entries.
Handles both container-list ((TARGET FONT ...) ...) and single-entry
\(TARGET FONT ...) forms.
Returns (NEW-ACC . REMAINING-ARGS).
Each parsed entry is stored as a plist (:target TARGET :font FONT :op OP)."
  (let ((tl (or current-acc (rapid-package--tl-new))))
    (cond
     ;; Container list: ((TARGET FONT) (TARGET FONT) ...)
     ((and (listp item) (consp (car item)) (not (keywordp (car item))))
      (dolist (entry item)
        (rapid-package--tl-append! tl
                                   (list :target (car entry)
                                         :font   (cadr entry)
                                         :op     (or (caddr entry) 'prepend))))
      (cons tl args))
     ;; Single entry: (TARGET FONT) or (TARGET FONT OP)
     ((consp item)
      (rapid-package--tl-append! tl
                                 (list :target (car item)
                                       :font   (cadr item)
                                       :op     (or (caddr item) 'prepend)))
      (cons tl args))
     (t
      (error "syntax error: invalid :rules entry: %S" item)))))

;;; Fontset Code Generator

(defun rapid-package--codegen-fontset-quote-target (target)
  "Return a quoted form of TARGET suitable for embedding in generated code."
  (cond
   ((symbolp target)  `',target)
   ((integerp target) target)
   ((consp target)    `',target)))

(defun rapid-package--codegen-fontset (name base size rules rescale default-p
                                            &optional variable)
  "Generate the fontset setup form for NAME.
BASE is the base font string or unquote form.
SIZE is an optional numeric size.
RULES is the list of (:target TARGET :font FONT :op OP) plists.
RESCALE is the list of (FONT-REGEXP RATIO) entries or nil.
DEFAULT-P is non-nil to set this fontset as the default face font.
VARIABLE is an optional list of (SYMBOL EXPR) let* bindings."
  (let* ((name-str     (symbol-name name))
         (fontset-name (concat "fontset-" name-str))
         (base-form    (rapid-package--codegen-unquote base))
         (base-spec    (if size
                           `(font-spec :family ,base-form :size ,size)
                         `(font-spec :family ,base-form)))
         (rule-forms
          (mapcar
           (lambda (rule)
             (let* ((target (plist-get rule :target))
                    (font   (rapid-package--codegen-unquote (plist-get rule :font)))
                    (add    (pcase (plist-get rule :op)
                              ('prepend ''prepend)
                              ('append  ''append)
                              ('replace 'nil))))
               `(set-fontset-font ,fontset-name
                                  ,(rapid-package--codegen-fontset-quote-target target)
                                  ,font nil ,add)))
           rules))
         (rescale-forms
          (when rescale
            (mapcar
             (lambda (entry)
               (let ((font-re (rapid-package--codegen-unquote
                               (plist-get entry :variable)))
                     (ratio   (rapid-package--codegen-unquote
                               (plist-get entry :value))))
                 `(setf (alist-get ,font-re face-font-rescale-alist
                                   nil nil #'equal)
                        ,ratio)))
             rescale)))
         (default-forms
          (when default-p
            `((set-frame-font ,fontset-name t)
              (setf (alist-get 'font default-frame-alist) ,fontset-name))))
         (body-forms
          `(;; Create fontset if absent.
            (unless (member ,fontset-name (fontset-list))
              (create-fontset-from-ascii-font ,base-form nil ,name-str))
            ;; Apply BASE to all unicode ranges.
            (set-fontset-font ,fontset-name 'unicode ,base-spec)
            ;; Apply rules sequentially.
            ,@rule-forms
            ;; Update rescale alist.
            ,@rescale-forms
            ;; Set as default face/frame font if requested.
            ,@default-forms)))
    (if variable
        `(let* ,(mapcar (lambda (e)
                          (list (plist-get e :variable)
                                (plist-get e :value)))
                        variable)
           ,@body-forms)
      `(progn ,@body-forms))))

;;; Fontset

(defun rapid-package-fontset--unquote-p (val)
  "Return non-nil if VAL is an unquote form (\\, EXPR)."
  (and (consp val) (eq (car val) '\,)))

(defvar rapid-package-fontset-schema
  `((:_head          . list)
    (:variable       . alist)
    (:base           . single)
    (:size           . single)
    (:rules          . (rapid-package-dsl-parse-fontset-rules
                        . rapid-package--tl-value))
    (:rescale        . alist)
    (:default        . flag)
    (:when           . single)
    (:unless         . single)
    (:when-system    . list)
    (:unless-system  . list)
    (:when-gui       . flag)
    (:when-tty       . flag)
    (:when-ge        . alist)
    (:when-gt        . alist)
    (:when-le        . alist)
    (:when-lt        . alist)
    (:when-eq        . alist)
    (:when-ne        . alist)
    (:when-p         . list)
    (:disabled       . flag))
  "DSL schema for `rapid-package-fontset' argument parsing.")

(defun rapid-package-fontset--parse-args (head-and-args)
  "Parse rapid-package-fontset HEAD-AND-ARGS using the DSL.
HEAD-AND-ARGS is (NAME [DOCSTRING] :key val ...).
Returns a plist with :_head, :base, :size, :rules, :rescale, :default,
:variable, and condition keys."
  (let* ((parsed    (condition-case err
                        (rapid-package-dsl-parse head-and-args rapid-package-fontset-schema)
                      (error
                       (let ((n (or (and (consp head-and-args) (car head-and-args))
                                    'fontset)))
                         (if rapid-package--loading-file
                             (rapid-package--abort 'fontset "%s:%d: %s: %s"
                                                   rapid-package--loading-file
                                                   rapid-package--loading-line
                                                   n
                                                   (error-message-string err))
                           (rapid-package--abort 'fontset "%s: %s"
                                                 n
                                                 (error-message-string err)))))))
         (head-cons (rapid-package--parse-head (plist-get parsed :_head)))
         (name      (car head-cons))
         (doc       (cdr head-cons))
         (base      (plist-get parsed :base))
         (size      (plist-get parsed :size))
         (rules     (plist-get parsed :rules))
         (rescale   (plist-get parsed :rescale))
         (default   (plist-get parsed :default))
         (variable  (cl-remove-if
                     (lambda (e) (null (plist-get e :variable)))
                     (or (plist-get parsed :variable) nil))))
    (unless base
      (rapid-package--abort name ":base is required"))
    (unless rules
      (rapid-package--abort name ":rules is required"))
    (list :_head          (if doc (list name doc) (list name))
          :base          base
          :size          size
          :rules         rules
          :rescale       rescale
          :default       default
          :variable      variable
          :when          (plist-get parsed :when)
          :unless        (plist-get parsed :unless)
          :when-system   (plist-get parsed :when-system)
          :unless-system (plist-get parsed :unless-system)
          :when-gui      (plist-get parsed :when-gui)
          :when-tty      (plist-get parsed :when-tty)
          :when-ge       (plist-get parsed :when-ge)
          :when-gt       (plist-get parsed :when-gt)
          :when-le       (plist-get parsed :when-le)
          :when-lt       (plist-get parsed :when-lt)
          :when-eq       (plist-get parsed :when-eq)
          :when-ne       (plist-get parsed :when-ne)
          :when-p        (plist-get parsed :when-p)
          :disabled      (plist-get parsed :disabled))))

(defun rapid-package-fontset--validate-target (name target)
  "Validate TARGET in a rule for fontset NAME.
TARGET must be a symbol (script), integer (character), or cons (range)."
  (unless (or (symbolp target)
              (integerp target)
              (and (consp target)
                   (integerp (car target))
                   (integerp (cdr target))))
    (rapid-package--abort
     name
     "Invalid TARGET %S: must be a script symbol, character integer, or (FROM . TO) cons"
     target)))

(defun rapid-package-fontset--validate (name base rules &optional size variable)
  "Validate NAME, BASE, RULES, SIZE, and VARIABLE for `rapid-package-fontset'."
  (unless (string-match-p "\\`[[:alnum:]_]+\\'" (symbol-name name))
    (rapid-package--abort
     name
     "NAME must contain only letters, digits, and underscores, got: %S"
     name))
  (unless (or (stringp base) (rapid-package-fontset--unquote-p base))
    (rapid-package--abort
     name ":base must be a string or unquote form (e.g. ,var), got: %S" base))
  (when size
    (unless (numberp size)
      (rapid-package--abort
       name ":size must be a number, got: %S" size)))
  (unless (listp rules)
    (rapid-package--abort name ":rules must be a list"))
  (dolist (rule rules)
    (unless (and (listp rule) (keywordp (car rule)) (plist-get rule :target))
      (rapid-package--abort
       name "Each rule must be a plist (:target TARGET :font FONT :op OP), got: %S" rule))
    (let* ((target (plist-get rule :target))
           (font   (plist-get rule :font))
           (op     (plist-get rule :op)))
      (rapid-package-fontset--validate-target name target)
      (unless (or (stringp font) (rapid-package-fontset--unquote-p font))
        (rapid-package--abort
         name "FONT in rule must be a string or unquote form (e.g. ,var), got: %S" font))
      (unless (memq op '(prepend append replace))
        (rapid-package--abort
         name "Operation must be prepend, append, or replace, got: %S" op))))
  (when variable
    (unless (listp variable)
      (rapid-package--abort name ":variable must be a list of (:variable NAME :value EXPR) entries"))
    (dolist (e variable)
      (unless (and (listp e)
                   (symbolp (plist-get e :variable)))
        (rapid-package--abort
         name ":variable entry must be (:variable SYMBOL :value EXPR), got: %S" e)))))

(defun rapid-package-fontset--expand-from-data (data)
  "Validate and expand fontset IR DATA plist to an executable form.
DATA must contain :_head (with name), :base, and :rules, and may include
:size, :rescale, :default, :variable, and condition keywords.
Wraps in (when CONDITION ...) unless CONDITION is t."
  (let* ((condition (rapid-package--check-condition data))
         (head-cons (rapid-package--parse-head (plist-get data :_head)))
         (name      (car head-cons))
         (base      (plist-get data :base))
         (size      (plist-get data :size))
         (rules     (plist-get data :rules))
         (rescale   (plist-get data :rescale))
         (default-p (plist-get data :default))
         (variable  (plist-get data :variable)))
    (rapid-package-fontset--validate name base rules size variable)
    (let ((body (rapid-package--codegen-fontset
                 name base size rules rescale default-p variable)))
      (if (eq condition t)
          body
        `(when ,condition ,body)))))

;;;###autoload
(defmacro rapid-package-fontset (name &rest args)
  "Define an Emacs fontset NAME with the given configuration.

\\=(rapid-package-fontset NAME
  [:variable ((VAR EXPR) ...)]
  :base STRING-OR-UNQUOTE
  [:size NUMBER]
  :rules (RULE ...)
  [:rescale (RESCALE-RULE ...)]
  [:default BOOL]
  [:when CONDITION]
  [:unless CONDITION]
  [:when-system SYSTEMS]
  [:unless-system SYSTEMS]
  [:when-gui BOOL]
  [:when-tty BOOL]
  [:when-ge ((VAR VAL) ...)]
  [:when-gt ((VAR VAL) ...)]
  [:when-le ((VAR VAL) ...)]
  [:when-lt ((VAR VAL) ...)]
  [:when-eq ((VAR VAL) ...)]
  [:when-ne ((VAR VAL) ...)]
  [:when-p PREDICATES]
  [:disabled BOOL])

NAME is a symbol; the generated fontset is named \"fontset-NAME\".

:variable (optional) — list of (SYMBOL EXPR) bindings evaluated before all
  other configuration.  Variables defined here can be referenced in
  :base, :rules, and :rescale using unquote syntax: ,VAR.
  Example: :variable ((cjk \"Noto Sans CJK JP\"))

:base (required) — base font family string or ,EXPR unquote.
  Pass only family names here (e.g. \"Iosevka\" or ,my-font).
  Do not encode font size in :base or in rule font names.

:rules (required) — list of (TARGET FONT) or (TARGET FONT OP) entries.
  TARGET: script symbol (e.g. \\='han), character (e.g. ?A), or range cons
    (e.g. \\='(#xe000 . #xf8ff)).
  FONT: font name string or ,EXPR unquote.
  OP: prepend (default), append, or replace.
    replace removes BASE from the candidate list for that range.

:size (optional) — numeric font size used when applying :base via
  `font-spec'.  Pass font size here.

:rescale (optional) — list of (FONT-REGEXP RATIO) entries.
  FONT-REGEXP may be a string or ,EXPR unquote.
  Updates `face-font-rescale-alist' (global; last definition wins).

:default (optional) — when t, sets this fontset as the default face font
  and adds it to `default-frame-alist'.

:when CONDITION — apply fontset only when CONDITION is non-nil.
:unless CONDITION — apply fontset only when CONDITION is nil.
:when-system SYSTEMS — apply only on specified system types (OR condition).
:unless-system SYSTEMS — apply except on specified system types (NAND).
:when-gui BOOL — apply only in a GUI (graphical) environment.
:when-tty BOOL — apply only in a TTY (terminal) environment.
:when-ge PAIRS — apply when >= comparisons hold: ((VAR VAL) ...).
:when-gt PAIRS — apply when > comparisons hold.
:when-le PAIRS — apply when <= comparisons hold.
:when-lt PAIRS — apply when < comparisons hold.
:when-eq PAIRS — apply when = comparisons hold.
:when-ne PAIRS — apply when /= comparisons hold.
:when-p PREDICATES — apply when all predicates return non-nil.
:disabled BOOL — when t, skip all code generation for this block."
  (declare (indent defun))
  (unless (symbolp name)
    (rapid-package--abort 'macro "NAME must be a symbol, got: %S" name))
  (rapid-package-fontset--expand-from-data
   (rapid-package-fontset--parse-args (cons name args))))

;;; Fontset JSON

(defun rapid-package-fontset--fill-json (data json-obj)
  "Fill JSON-OBJ with fontset DATA fields.
Uses `rapid-package--plist-to-json-generic' with fontset schema for
flag detection (`:default') and IR type resolution (`:rules', `:rescale').
Does not set the \"type\" field; the caller is responsible for that."
  (rapid-package--plist-to-json-generic
   data "name" json-obj rapid-package-fontset-schema))

(defun rapid-package-fontset--to-json (data)
  "Serialize fontset IR plist DATA to a JSON hash-table.
DATA is a plist produced by `rapid-package-fontset--parse-args'.
Returns a hash-table suitable for `json-encode'.
The output shape matches the shared item contract used by
`rapid-package--export-json' (type-tagged object in `items')."
  (let ((obj (make-hash-table :test 'equal)))
    (puthash "type" "fontset" obj)
    (rapid-package-fontset--fill-json data obj)
    obj))

(defun rapid-package-fontset--from-json (item)
  "Deserialize a fontset JSON hash-table ITEM into fontset IR plist.
ITEM is a hash-table as returned by `json-read'.
Returns a plist compatible with `rapid-package-fontset--expand-from-data'."
  (rapid-package--json-to-parsed item rapid-package-fontset-schema))

(provide 'rapid-package-fontset)

;;; rapid-package-fontset.el ends here
