;;; rapid-package-tl.el --- Internal tail-tracked list builder -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cottontailia
;; Additional-Author: AI Assistant
;; URL: https://github.com/cottontailia/rapid-package
;; License: CC0

;;; Commentary:

;; Internal O(1)-append list builder used by rapid-package.
;; Stores (HEAD . TAIL) where HEAD is the first cons cell and TAIL is the last.
;; append!/extend! take ownership of the cons cells they receive; pass
;; (copy-sequence ITEMS) if the caller needs to retain ITEMS independently.

;;; Code:

(require 'cl-lib)

(cl-defstruct (rapid-package--tl
               (:constructor rapid-package--tl--make (head tail)))
  "Tail-tracked list builder.
HEAD is the first cons cell, TAIL is the last cons cell."
  head tail)

(defun rapid-package--tl-new ()
  "Create and return an empty tail-list builder."
  (rapid-package--tl--make nil nil))

(defun rapid-package--tl-empty-p (tl)
  "Return non-nil if TL has no elements."
  (null (rapid-package--tl-head tl)))

(defun rapid-package--tl--last-cell (lst)
  "Return the last cons cell of LST, or nil if LST is nil."
  (when lst
    (let ((p lst))
      (while (cdr p)
        (setq p (cdr p)))
      p)))

(defun rapid-package--tl-append! (tl item)
  "Append ITEM to TL (destructively) and return TL.
This is O(1)."
  (let ((cell (cons item nil)))
    (if (rapid-package--tl-empty-p tl)
        (setf (rapid-package--tl-head tl) cell
              (rapid-package--tl-tail tl) cell)
      (setcdr (rapid-package--tl-tail tl) cell)
      (setf (rapid-package--tl-tail tl) cell)))
  tl)

(defun rapid-package--tl-prepend! (tl item)
  "Prepend ITEM to the front of TL (destructively) and return TL.
This is O(1).

Use this to insert at the head, e.g. for :before position semantics."
  (let ((was-empty (rapid-package--tl-empty-p tl))
        (cell (cons item (rapid-package--tl-head tl))))
    (setf (rapid-package--tl-head tl) cell)
    (when was-empty
      (setf (rapid-package--tl-tail tl) cell)))
  tl)

(defun rapid-package--tl-prepend-extend! (tl items)
  "Prepend ITEMS (a proper list) to the front of TL (destructively) and return TL.
If ITEMS is nil, TL is returned unchanged.  This is O(length ITEMS).

The items appear in the same order as ITEMS at the front of TL.

TL takes ownership of ITEMS's cons cells: any subsequent mutation of
TL (e.g. via `rapid-package--tl-append!') will also modify the tail of
ITEMS.  ITEMS must therefore be a freshly-created list that will not be
referenced after this call.  To keep ITEMS independent, pass
\(copy-sequence ITEMS) instead."
  (when items
    (let ((items-tail (rapid-package--tl--last-cell items)))
      (setcdr items-tail (rapid-package--tl-head tl))
      (setf (rapid-package--tl-head tl) items)
      (when (null (rapid-package--tl-tail tl))
        (setf (rapid-package--tl-tail tl) items-tail))))
  tl)

(defun rapid-package--tl-extend! (tl items)
  "Append ITEMS (a proper list) to TL (destructively) and return TL.
If ITEMS is nil, TL is returned unchanged.

TL takes ownership of ITEMS's cons cells: any subsequent mutation of
TL (e.g. via `rapid-package--tl-append!') will also modify the tail of
ITEMS.  ITEMS must therefore be a freshly-created list that will not be
referenced after this call.  To keep ITEMS independent, pass
\(copy-sequence ITEMS) instead."
  (when items
    (let ((items-tail (rapid-package--tl--last-cell items)))
      (if (rapid-package--tl-empty-p tl)
          (setf (rapid-package--tl-head tl) items)
        (setcdr (rapid-package--tl-tail tl) items))
      (setf (rapid-package--tl-tail tl) items-tail)))
  tl)


(defun rapid-package--tl-concat! (dst src)
  "Destructively splice SRC onto the end of DST and return DST.
This operation is O(1): it links DST's tail to SRC's head and
updates DST's tail.

After concatenation, SRC is reset to empty to help avoid accidental reuse.

Note: this shares structure (by linking cons cells).  Do not mutate
the returned list in ways that assume DST or SRC will remain independent."
  (cond
   ((rapid-package--tl-empty-p src)
    dst)
   ((rapid-package--tl-empty-p dst)
    (setf (rapid-package--tl-head dst) (rapid-package--tl-head src)
          (rapid-package--tl-tail dst) (rapid-package--tl-tail src)))
   (t
    (setcdr (rapid-package--tl-tail dst) (rapid-package--tl-head src))
    (setf (rapid-package--tl-tail dst) (rapid-package--tl-tail src))))
  (setf (rapid-package--tl-head src) nil
        (rapid-package--tl-tail src) nil)
  dst)

(defun rapid-package--tl-from (items)
  "Create a tl that wraps the existing list ITEMS.
This is O(n) due to finding the tail, but avoids copying cons cells.
TL takes ownership of ITEMS's cons cells; do not modify ITEMS afterwards."
  (rapid-package--tl--make items (rapid-package--tl--last-cell items)))

(defun rapid-package--tl-value (tl)
  "Return the built list from TL.
This is the actual list structure maintained by TL."
  (rapid-package--tl-head tl))

(provide 'rapid-package-tl)

;;; rapid-package-tl.el ends here
