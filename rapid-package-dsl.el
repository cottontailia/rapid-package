;;; rapid-package-dsl.el --- DSL parser for rapid-package -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cottontailia

;; Author: Cottontailia
;; Additional-Author: AI Assistant
;; URL: https://github.com/cottontailia/rapid-package
;; License: CC0

;;; Commentary:

;; A small, composable DSL parser used by rapid-package.
;;
;; This file provides the core parsing engine and shared infrastructure.
;; Keyword-specific custom parsers (:bind, :unbind, :hook, :with) live in
;; rapid-package-dsl-parse.el, which requires this file.
;;
;; This parser converts keyword-based configuration forms into a
;; normalized plist representation according to a schema.
;;
;; It is intentionally syntax-focused:
;;  - It validates structure only.
;;  - It does not interpret semantics.
;;  - All higher-level meaning is delegated to the caller.
;;
;; Supported schema types:
;;   single   - exactly one value
;;   list     - one or more values
;;   flag     - boolean flag
;;   body     - arbitrary forms until next keyword
;;   alist    - association-style entries (use-package compatible)
;;   (PARSER-FN . FINALIZE-FN) - custom parser with explicit finalize
;;   PARSER-FN  - custom parser; finalize defaults to rapid-package--tl-value
;;
;; The goal is to provide a predictable and minimal parsing layer
;; without embedding policy or behavior.
;;
;; --- Design note: ordered accumulation ---
;;
;; All list-like values (list, body, alist, and function types) are
;; accumulated in insertion order throughout parsing.  There is no
;; "accumulate in reverse then nreverse at the end" convention.
;;
;; The mechanism is a `rapid-package--tl' builder per key:
;;
;;   result-tails  - plist mapping key -> the rapid-package--tl builder
;;                   for that key's value list.
;;
;; Appending a new element is O(1) via `rapid-package--tl-append!'.
;;
;; Custom parser functions receive (ITEM ARGS CURRENT-KEY CURRENT-ACC) and
;; must return (NEW-ACC . REMAINING-ARGS).  CURRENT-ACC is nil on the first
;; call and the parser's own accumulator structure on subsequent calls.
;; After the last item, `rapid-package-dsl-parse' finalizes each
;; function-type key by calling the FINALIZE-FN from its schema type, or
;; `rapid-package--tl-value' if the schema type is a bare function.

;;; Code:

(require 'cl-lib)
(require 'rapid-package-tl)

;;; Normalization Helpers

(defun rapid-package-dsl--normalize-pair (entry key-prop value-prop &optional quote-value)
  "Normalize ENTRY into a plist representation.

ENTRY may be:
  (KEY . VALUE)
  (KEY VALUE)
  (KEY VALUE DOC)

KEY-PROP and VALUE-PROP specify the property names to use.
If QUOTE-VALUE is non-nil, VALUE is processed via
`rapid-package-dsl-quote'.

Returns a plist:
  (KEY-PROP KEY VALUE-PROP VALUE [:description DOC])."
  (cond
   ;; Dotted pair (key . value) - cdr is not a cons (but may be nil)
   ;; Check: no cadr (either atom cdr, or nil cdr which makes it look like a list)
   ((and (consp entry) (not (consp (cdr entry))))
    (list key-prop (car entry)
          value-prop (if quote-value
                         (rapid-package-dsl-quote (cdr entry))
                       (cdr entry))))

   ;; 3-element form (key value "description")
   ;; cddr non-nil and cdddr nil avoids full-list traversal of length
   ((and (consp entry) (cddr entry) (null (cdddr entry)))
    (list key-prop (car entry)
          value-prop (if quote-value
                         (rapid-package-dsl-quote (cadr entry))
                       (cadr entry))
          :description (caddr entry)))

   ;; 2-element form (key value)
   ;; cdr non-nil and cddr nil
   ((and (consp entry) (cdr entry) (null (cddr entry)))
    (list key-prop (car entry)
          value-prop (if quote-value
                         (rapid-package-dsl-quote (cadr entry))
                       (cadr entry))))

   (t (error "DSL syntax error: invalid entry format: %S" entry))))

(defun rapid-package-dsl--normalize-alist-item (item)
  "Normalize an alist ITEM to plist format.

Handles:
  - (var . val)
  - (var val)
  - (var val \"description\")

Values are processed through `rapid-package-dsl-quote' for proper
unquote support.

Returns: (:variable VAR :value VALUE [:description DOC])."
  (rapid-package-dsl--normalize-pair item :variable :value t))


(defun rapid-package-dsl-parse (args schema)
  "Parse ARGS according to SCHEMA and return a plist.
The parser is stateful and processes ARGS sequentially.
Schema entries define structural expectations only.
No semantic validation is performed.
Keyword order is preserved for list-like types.

SCHEMA is an alist of (KEYWORD . TYPE) entries where TYPE can be:
  - `single'  : Requires exactly one value
  - `list'    : Requires at least one value (collected into a list)
                Quoted values are automatically unwrapped:
                  \\='symbol -> symbol
                  \\='(a b) -> a, b (flattened)
                This allows natural Emacs Lisp syntax like:
                  :after \\='magit or :when-system \\='windows-nt
  - `flag'    : Optional boolean (0 or nil is false, otherwise true)
  - `body'    : Consumes all expressions until next keyword
  - `alist'   : Consumes pairs until next keyword
  - FUNCTION  : Custom parser function

Special SCHEMA entry:
  - (:_head . TYPE) : If present, ARGS must start with at least one value

Returns a plist with all parsed configuration.

Example:
  (rapid-package-dsl-parse \\='(magit \"Git\" :ensure t)
                          \\='((:_head . list) (:ensure . flag)))
  => (:_head (magit \"Git\") :ensure t)

--- Accumulation strategy ---

All list-like values are accumulated in insertion order using
tail-tracked lists (`rapid-package--tl-*').  There is no
\"reverse at the end\" step.

Custom parser functions (function type) receive CURRENT-ACC as the
raw tail-tracked structure from a prior call (or nil on first call),
and must return (NEW-ACC . REMAINING-ARGS) in the same form.
After the last item, `rapid-package-dsl-parse' calls
`rapid-package-dsl--finalize-groups' on function-type keys that
produce group lists (currently :bind and :unbind), or extracts
the head list for other accumulators."
  (let* ((head-config (assq :_head schema))
         (result nil)
         (result-tails nil)   ; plist: key -> TailList for that key's value
         (function-keys nil)  ; keys whose type is a custom parser function
         (current-key (and head-config :_head))
         (current-type (and head-config (cdr head-config)))
         ;; Direct pointer to the TailList of the current list-like key.
         ;; Avoids a plist-get on result-tails for every appended value.
         (current-tl nil))

    ;; Register :_head in tail tracking if it's a list-like type
    (when (memq current-type '(list body alist))
      (let ((tl (rapid-package--tl-new)))
        (setq result-tails (plist-put result-tails :_head tl)
              current-tl tl)))

    ;; Helper: append VALUE to the current key's TailList.
    ;; plist-put into result is called only on the first append (empty->non-empty);
    ;; subsequent appends mutate existing cons cells via tl-append!, so the
    ;; pointer already stored in result remains valid without re-registering.
    (cl-labels
        ((append-to-key (value)
           (let ((was-empty (rapid-package--tl-empty-p current-tl)))
             (rapid-package--tl-append! current-tl value)
             (when was-empty
               (setq result (plist-put result current-key
                                       (rapid-package--tl-value current-tl))))))

         (init-list-key (key)
           (let ((existing (plist-get result-tails key)))
             (if existing
                 (setq current-tl existing)
               (let ((tl (rapid-package--tl-new)))
                 (setq result-tails (plist-put result-tails key tl)
                       current-tl tl))))))

      (while args
        (let* ((item (pop args))
               (entry (and (keywordp item) (assq item schema))))
          (cond
           ;; New keyword
           (entry
            ;; Validate that the previous keyword got its required value(s)
            (when current-key
              (cond
               ((and (eq current-type 'single)
                     (not (plist-member result current-key)))
                (error "DSL syntax error: keyword %S expects a value"
                       current-key))
               ((and (memq current-type '(list body alist))
                     (not (plist-get result current-key)))
                (error "DSL syntax error: keyword %S expects at least one value"
                       (if (eq current-key :_head) "Initial position" current-key)))))

            (let ((type (cdr entry)))
              (cond
               ;; Flag: process immediately, no pending state.
               ;; Must be checked before functionp, because some type
               ;; symbols like 'list happen to be bound as Emacs built-in
               ;; functions and would be misidentified as custom parsers.
               ((eq type 'flag)
                (let ((val (if (and args (not (keywordp (car args))))
                               (pop args)
                             t)))
                  (setq result (plist-put result item
                                          (and val (not (and (numberp val)
                                                             (<= val 0)))))
                        current-key nil
                        current-type nil
                        current-tl nil)))

               ;; single: no accumulator needed
               ((eq type 'single)
                (setq current-key item
                      current-type 'single
                      current-tl nil))

               ;; list / body / alist: initialize TailList accumulator
               ((memq type '(list body alist))
                (setq current-key item
                      current-type type)
                (init-list-key item))

               ;; Custom parser: (PARSER-FN . FINALIZE-FN) cons, or bare PARSER-FN.
               ;; Bare PARSER-FN defaults to rapid-package--tl-value as finalize.
               ;; Checked after known type symbols to avoid misidentifying built-in
               ;; function symbols (e.g. 'list) as custom parsers.
               ((or (functionp type)
                    (and (consp type) (functionp (car type)) (functionp (cdr type))))
                (setq current-key item
                      current-type type
                      current-tl nil)
                (cl-pushnew item function-keys))

               (t
                (error "DSL internal error: unknown schema type %S for keyword %S"
                       type item)))))

           ;; Unknown keyword
           ((keywordp item)
            (error "DSL syntax error: unknown keyword %S. Allowed: %S"
                   item (mapcar #'car schema)))

           ;; Value
           (t
            (if (null current-key)
                (error "DSL syntax error: unexpected value %S" item)
              (cond
               ;; single: exactly one value, then reset state
               ((eq current-type 'single)
                (setq result (plist-put result current-key item)
                      current-key nil
                      current-type nil
                      current-tl nil))

               ;; list: flat-append atoms or lists, O(1) via tail tracking
               ((eq current-type 'list)
                (cond
                 ;; Quoted value: unwrap quote and add the symbol
                 ((and (listp item) (eq (car item) 'quote) (consp (cdr item)))
                  (let ((quoted-val (cadr item)))
                    (if (listp quoted-val)
                        (dolist (elem quoted-val)
                          (append-to-key elem))
                      (append-to-key quoted-val))))
                 ;; Regular list: flatten
                 ((listp item)
                  (dolist (elem item)
                    (append-to-key elem)))
                 ;; Atom: add as-is
                 (t
                  (append-to-key item))))

               ;; body: each form is one element, O(1) via tail tracking
               ((eq current-type 'body)
                (append-to-key item))

               ;; alist: normalize each entry, O(1) via tail tracking
               ((eq current-type 'alist)
                (cond
                 ;; Container list: ((k1 . v1) (k2 . v2) ...)
                 ((and (listp item)
                       (consp (car item))
                       (not (keywordp (car (car item)))))
                  (dolist (pair item)
                    (if (consp pair)
                        (append-to-key
                         (rapid-package-dsl--normalize-alist-item pair))
                      (error "DSL syntax error: invalid entry for keyword %S: %S"
                             current-key pair))))

                 ;; Single pair
                 ((consp item)
                  (append-to-key
                   (rapid-package-dsl--normalize-alist-item item)))

                 ;; Symbol with no value
                 ((symbolp item)
                  (append-to-key (list :variable item :value nil)))

                 (t
                  (error "DSL syntax error: invalid entry for keyword %S: %S"
                         current-key item))))

               ;; Custom parser: extract parser-fn from cons or bare function,
               ;; then delegate item + remaining args to it.
               ((or (functionp current-type)
                    (and (consp current-type) (functionp (car current-type))))
                (let* ((parser-fn   (if (consp current-type)
                                        (car current-type)
                                      current-type))
                       (current-acc (plist-get result current-key))
                       (parse-result
                        (funcall parser-fn item args current-key current-acc))
                       (new-acc  (car parse-result))
                       (new-args (cdr parse-result)))
                  (setq args   new-args)
                  (setq result (plist-put result current-key new-acc))))

               (t
                (error "DSL internal error: unknown type %S for keyword %S"
                       current-type current-key))))))))

      ;; Final validation
      (when current-key
        (cond
         ((and (eq current-type 'single)
               (not (plist-member result current-key)))
          (error "DSL syntax error: keyword %S requires a value, but input ended"
                 current-key))
         ((and (memq current-type '(list body alist))
               (not (plist-get result current-key)))
          (error "DSL syntax error: keyword %S requires at least one value"
                 (if (eq current-key :_head) :initial-position current-key)))))

      ;; Finalize custom-parser keys.
      ;; Schema type is either (PARSER-FN . FINALIZE-FN) or bare PARSER-FN.
      ;; Bare PARSER-FN defaults to rapid-package--tl-value as finalize.
      ;; The finalize function receives the raw acc and returns the external IR.
      (dolist (key function-keys)
        (let ((acc (plist-get result key)))
          (when acc
            (let* ((type        (cdr (assq key schema)))
                   (finalize-fn (if (consp type)
                                    (cdr type)
                                  #'rapid-package--tl-value)))
              (setq result (plist-put result key (funcall finalize-fn acc))))))))

    result))

;;; Quoting Utilities

(defun rapid-package-dsl-quote (val)
  "Quote VAL for macro expansion with unquote support.

Rules:
  (\\, X)        -> X
  (quote X)     -> unchanged
  (function X)  -> unchanged  (#\\='X passes through as-is)
  self-evaluating -> unchanged
  otherwise     -> (quote VAL)"
  (cond
   ;; Unquote: ,value -> evaluate it
   ((and (consp val) (eq (car val) 'unquote)) (cadr val))
   ;; Already quoted
   ((and (listp val) (eq (car val) 'quote)) val)
   ;; Function reference: #'foo -> (function foo) -> pass through
   ((and (consp val) (eq (car val) 'function)) val)
   ;; Self-evaluating types
   ((or (keywordp val) (numberp val) (stringp val) (booleanp val)) val)
   (t `',val)))

(defun rapid-package-dsl-quote-list (lst)
  "Apply `rapid-package-dsl-quote' to all elements in LST.

Returns a `(list ...)' form suitable for macro expansion.
Returns nil for empty lists."
  (if (null lst) nil `(list ,@(mapcar #'rapid-package-dsl-quote lst))))

(provide 'rapid-package-dsl)

;;; rapid-package-dsl.el ends here
