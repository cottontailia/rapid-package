;;; rapid-package-inspector.el --- Inspect package symbols and configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cottontailia

;; Author: Cottontailia
;; Additional-Author: AI Assistant
;; URL: https://github.com/cottontailia/rapid-package
;; License: CC0

;;; Commentary:

;; Inspect packages by showing:
;;  - Package summary/description
;;  - Loaded symbols (User Options, Commands, Hooks, Modes, Keymaps) using prefix scan
;;
;; Entry point: M-x rapid-package-inspector
;;
;; Usage:
;;  - Inside (rapid-package NAME ...) -> auto-detects NAME
;;  - Inside (rapid-package-after PACKAGE ...) -> auto-detects PACKAGE
;;  - Inside (use-package NAME ...) -> auto-detects NAME
;;  - Anywhere else -> prompts for package name
;;
;; All symbols are clickable buttons that open standard Help descriptions.
;; Uses help-mode, so all standard Help features work (links, history, etc).

;;; Code:

(require 'help-mode)
(require 'package)

;;; Customization

(defgroup rapid-package-inspector nil
  "Inspect rapid-package declarations and loaded symbols."
  :group 'rapid-package
  :prefix "rapid-package-inspector-")

(defcustom rapid-package-inspector-include-modes-and-maps t
  "If non-nil, also collect major/minor modes and keymaps in loaded symbol scan."
  :type 'boolean
  :group 'rapid-package-inspector)

(defcustom rapid-package-inspector-value-preview-chars 80
  "Maximum characters for variable value preview in symbol list."
  :type 'integer
  :group 'rapid-package-inspector)

;;; Helper Functions - Context Inference

(defun rapid-package-inspector--symbol-at-point-prefix ()
  "Extract the prefix from symbol at point (before first dash).
Returns the prefix string, or nil if no symbol or no dash found."
  (let ((sym (symbol-at-point)))
    (when sym
      (let* ((s (symbol-name sym))
             (i (cl-position ?- s)))
        (when i (substring s 0 i))))))

(defun rapid-package-inspector--find-enclosing-sexp ()
  "Find innermost enclosing sexp starting with rapid-package-* or use-package.
Returns the form as a list, or nil if not found."
  (save-excursion
    (let ((start-pos (point))
          (found nil))
      (condition-case nil
          (progn
            (while (and (not found) (not (bobp)))
              (condition-case nil
                  (backward-up-list 1)
                (scan-error (goto-char (point-min))))

              (when (looking-at "(")
                (let* ((sexp-start (point))
                       (sexp-end (save-excursion
                                   (ignore-errors
                                     (forward-sexp 1)
                                     (point))))
                       (form (ignore-errors 
                               (save-excursion
                                 (read (current-buffer))))))
                  (when (and (consp form)
                             (memq (car form) '(rapid-package rapid-package-after use-package))
                             sexp-end
                             (<= sexp-start start-pos)
                             (< start-pos sexp-end))
                    (setq found form))))))
        (error nil))
      found)))

(defun rapid-package-inspector--infer-from-rapid-package ()
  "If inside (rapid-package NAME ...) or (rapid-package-after NAME ...),
return NAME as string."
  (let ((form (rapid-package-inspector--find-enclosing-sexp)))
    (when form
      (let ((macro-name (car form)))
        (cond
         ((eq macro-name 'rapid-package)
          (let ((nm (cadr form)))
            (cond ((symbolp nm) (symbol-name nm))
                  ((stringp nm) nm))))
         ((eq macro-name 'rapid-package-after)
          (let ((nm (cadr form)))
            (cond ((symbolp nm) (symbol-name nm))
                  ((stringp nm) nm)))))))))

(defun rapid-package-inspector--infer-from-use-package ()
  "If point is inside (use-package NAME ...), return NAME as string."
  (let ((form (rapid-package-inspector--find-enclosing-sexp)))
    (when (and form (eq (car form) 'use-package))
      (let ((nm (cadr form)))
        (cond ((symbolp nm) (symbol-name nm))
              ((stringp nm) nm))))))

(defun rapid-package-inspector--infer-target (&optional explicit)
  "Infer the target package name from context.
If EXPLICIT is provided, use it.  Otherwise, try to detect from:
  1. Enclosing rapid-package or rapid-package-after form
  2. Enclosing use-package form
  3. Prefix of symbol at point
Returns the package name as a string, or nil."
  (or explicit
      (rapid-package-inspector--infer-from-rapid-package)
      (rapid-package-inspector--infer-from-use-package)
      (rapid-package-inspector--symbol-at-point-prefix)))

;;; Helper Functions - Symbol Collection

(defun rapid-package-inspector--first-line (s)
  "Return the first line of string S, or nil if S is not a string."
  (when (and s (stringp s))
    (car (split-string s "\n" t))))

(defun rapid-package-inspector--custom-variable-p (sym)
  "Return non-nil if SYM is a custom variable."
  (and (symbolp sym) (fboundp 'custom-variable-p) (custom-variable-p sym)))

(defun rapid-package-inspector--minor-mode-p (sym)
  "Return non-nil if SYM is a loaded minor mode command."
  (and (fboundp sym)
       (commandp sym)
       (not (autoloadp (symbol-function sym)))
       (boundp sym)))

(defun rapid-package-inspector--major-mode-p (sym)
  "Return non-nil if SYM looks like a major mode command."
  (and (fboundp sym)
       (commandp sym)
       (string-suffix-p "-mode" (symbol-name sym))
       (not (autoloadp (symbol-function sym)))
       (not (boundp sym))))

(defun rapid-package-inspector--mode-map-symbol (mode-sym)
  "Return MODE-SYM's conventional keymap variable symbol, if it exists."
  (let ((m (intern (format "%s-map" (symbol-name mode-sym)))))
    (when (and (boundp m) (keymapp (symbol-value m)))
      m)))

(defun rapid-package-inspector--value-preview (val &optional sym)
  "Return a short preview string of VAL for inline display.
If SYM is non-nil, use it to improve formatting decisions."
  (let* ((name (and sym (symbol-name sym)))
         (looks-like-key
          (and name
               (string-match-p
                "\\(key\\|kbd\\|prefix\\|map\\)"
                name)))
         (raw
          (cond
           ;; Keymap
           ((keymapp val)
            "<keymap>")

           ;; Key sequence (if variable name suggests it)
           ((and looks-like-key
                 (or (stringp val) (vectorp val)))
            (condition-case nil
                (key-description val)
              (error (prin1-to-string val))))

           ;; Default
           (t
            (prin1-to-string val))))
         (s (if (> (length raw) rapid-package-inspector-value-preview-chars)
                (concat (substring raw 0 (- rapid-package-inspector-value-preview-chars 3)) "...")
              raw)))
    s))

(defun rapid-package-inspector--file-first-line-summary (file)
  "Extract summary from first line of FILE.
Returns summary string or nil."
  (when (and file (file-exists-p file))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file nil 0 300)
          (goto-char (point-min))
          ;; Match: ;;; filename.el --- Summary
          (when (looking-at "^;;;? +[^ ]+\\.el +--- +\\(.+?\\) *\\(-\\*-.*\\)?$")
            (string-trim (match-string 1))))
      (error nil))))

(defun rapid-package-inspector--package-summary (package)
  "Get summary/description for PACKAGE (string or symbol).
Returns summary string or nil."
  (let* ((pkg-sym (if (symbolp package) package (intern package)))
         (pkg-name (symbol-name pkg-sym))
         summary)

    ;; Try 1: package-alist (for installed packages)
    (when-let ((desc (cadr (assq pkg-sym package-alist))))
      (let ((sum (package-desc-summary desc)))
        (when (and sum
                   (not (string-empty-p sum))
                   (not (string-equal sum "No description available.")))
          (setq summary sum))))

    ;; Try 2: group-documentation property
    (unless summary
      (let ((doc (get pkg-sym 'group-documentation)))
        (when (and doc (not (string-empty-p doc)))
          (setq summary doc))))

    ;; Try 3: first line of file
    (unless summary
      (when-let ((file (locate-library pkg-name)))
        (setq summary (rapid-package-inspector--file-first-line-summary file))))

    summary))

(defun rapid-package-inspector--symbol-status (sym)
  "Return load status of SYM: \\='loaded, \\='autoload, or \\='unknown."
  (cond
   ;; Function/command
   ((fboundp sym)
    (if (autoloadp (symbol-function sym))
        'autoload
      'loaded))
   ;; Variable
   ((boundp sym) 'loaded)
   ;; Unknown
   (t 'unknown)))

(defun rapid-package-inspector--collect-loaded (pkg)
  "Collect loaded symbols via prefix scan.
Returns plist of lists: :user-options :internal-vars
:cmds :hooks and optionally :major :minor :maps."
  (let* ((prefix (concat pkg "-"))
         (global-prefix (concat "global-" pkg "-"))
         (user-options '())
         (internal-vars '())
         (cmds '())
         (hooks '())
         (major '())
         (minor '())
         (maps '()))
    (mapatoms
     (lambda (s)
       (let ((name (symbol-name s)))
         (when (or (string-prefix-p prefix name)
                   (string-prefix-p global-prefix name))
           (let* ((is-bound (boundp s))
                  (is-custom (rapid-package-inspector--custom-variable-p s))
                  (is-command (commandp s))
                  (is-hook (and is-bound
                                (or (string-suffix-p "-hook" name)
                                    (string-suffix-p "-functions" name))))
                  (is-keymap (and is-bound
                                  (keymapp (symbol-value s))
                                  (or (string-suffix-p "-map" name)
                                      (string-suffix-p "-mode-map" name)))))
             ;; Categories are not mutually exclusive.
             ;; In particular, defcustom-defined hooks/functions should be shown
             ;; in both User Options and Hooks.
             (when is-custom
               (push s user-options))

             (when is-command
               (push s cmds))

             (when is-hook
               (push s hooks))

             ;; Internal Variables (defvar but not defcustom / command / hook)
             (when (and is-bound
                        (not is-custom)
                        (not is-command)
                        (not is-hook))
               (push s internal-vars))

             (when rapid-package-inspector-include-modes-and-maps
               (when (and (string-suffix-p "-mode" name)
                          (fboundp s)
                          is-command)
                 (cond
                  ((rapid-package-inspector--minor-mode-p s)
                   (push s minor)
                   (when-let ((ms (rapid-package-inspector--mode-map-symbol s)))
                     (push ms maps)))
                  ((rapid-package-inspector--major-mode-p s)
                   (push s major)
                   (when-let ((ms (rapid-package-inspector--mode-map-symbol s)))
                     (push ms maps)))))

               (when is-keymap
                 (push s maps))))))))

    (let ((sym-name-less (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
      (list :user-options (sort (delete-dups user-options) sym-name-less)
            :internal-vars (sort (delete-dups internal-vars) sym-name-less)
            :cmds  (sort (delete-dups cmds)  sym-name-less)
            :hooks (sort (delete-dups hooks) sym-name-less)
            :major (sort (delete-dups major) sym-name-less)
            :minor (sort (delete-dups minor) sym-name-less)
            :maps  (sort (delete-dups maps)  sym-name-less)))))

;;; Main Function

;;;###autoload
(defun rapid-package-inspector (&optional pkg)
  "Inspect a package and show its symbols in *Help* buffer.
If PKG is nil, infer from the current context or prompt the user.
Displays package summary, User Options, Commands, Hooks, Modes, and Keymaps.
All symbols are displayed as clickable buttons."
  (interactive)
  (when (and (called-interactively-p 'any)
             (not (require 'rapid-package nil t)))
    (message "Note: rapid-package is not loaded. Showing package information."))

  (let* ((inferred (rapid-package-inspector--infer-target pkg))
         (target (if inferred
                     inferred
                   (completing-read "Package name: " 
                                    obarray
                                    (lambda (sym)
                                      (and (symbolp sym)
                                           (string-match-p "-" (symbol-name sym))))
                                    nil nil nil nil
                                    (when-let ((sym (symbol-at-point)))
                                      (let ((name (symbol-name sym)))
                                        (when (string-match "^\\([^-]+\\)" name)
                                          (match-string 1 name))))))))

    (unless (and target (stringp target) (not (string-empty-p target)))
      (user-error "No package name specified"))

    (when (and (called-interactively-p 'interactive)
               (not (featurep (intern target))))
      (when (y-or-n-p (format "Package '%s' is not loaded. Load it now? " target))
        (unless (require (intern target) nil t)
          (message "Could not load package '%s'" target))))

    ;; Display in Help buffer
    (let* ((loaded (rapid-package-inspector--collect-loaded target))
           (user-options (plist-get loaded :user-options))
           (internal-vars (plist-get loaded :internal-vars))
           (cmds (plist-get loaded :cmds))
           (hooks (plist-get loaded :hooks))
           (major (plist-get loaded :major))
           (minor (plist-get loaded :minor))
           (maps  (plist-get loaded :maps)))

      (with-help-window (help-buffer)
        ;; Setup cross-reference so help-go-back works
        (help-setup-xref (list #'rapid-package-inspector target)
                         (called-interactively-p 'interactive))

        (with-current-buffer standard-output
          ;; Title
          (insert (propertize (format "Package: %s\n" target)
                              'face 'bold))

          ;; Summary
          (when-let ((summary (rapid-package-inspector--package-summary target)))
            (insert (propertize (format "%s\n" summary)
                                'face 'italic)))

          (insert "\n")

          ;; Section header
          (insert (propertize 
                   "Note: Symbols are detected based on package name prefix.\n"
                   'face 'shadow))
          (insert (propertize 
                   "      If package is not loaded, information may be incomplete.\n"
                   'face 'shadow))
          (insert "\n")

          ;; User Options (with values)
          (insert (propertize (format "User Options (%d):\n" (length user-options))
                              'face 'italic))
          (if (null user-options)
              (insert "  (none)\n")
            (dolist (var user-options)
              (insert "  ")
              (insert-button (symbol-name var)
                             'type 'help-variable
                             'help-args (list var))

              ;; Add value preview
              (condition-case _
                  (let ((val (symbol-value var)))
                    (insert (propertize 
                             (format "  %s" 
                                     (rapid-package-inspector--value-preview val var))
                             'face 'shadow)))
                (error 
                 (insert (propertize "  <unbound>" 'face 'shadow))))

              ;; Add status
              (let ((status (rapid-package-inspector--symbol-status var)))
                (unless (eq status 'loaded)
                  (insert (propertize 
                           (format " [%s]" status)
                           'face 'shadow))))

              ;; Add docstring first line
              (when-let ((doc (documentation-property 
                               var 'variable-documentation t)))
                (insert (format "\n      %s" (rapid-package-inspector--first-line doc))))

              (insert "\n")))
          (insert "\n")

          ;; Internal Variables (no values)
          (insert (propertize (format "Internal Variables (%d):\n" (length internal-vars))
                              'face 'italic))
          (if (null internal-vars)
              (insert "  (none)\n")
            (dolist (var internal-vars)
              (insert "  ")
              (insert-button (symbol-name var)
                             'type 'help-variable
                             'help-args (list var))

              ;; Add status
              (let ((status (rapid-package-inspector--symbol-status var)))
                (unless (eq status 'loaded)
                  (insert (propertize 
                           (format " [%s]" status)
                           'face 'shadow))))

              ;; Add docstring first line
              (when-let ((doc (documentation-property 
                               var 'variable-documentation t)))
                (insert (format "\n      %s" (rapid-package-inspector--first-line doc))))

              (insert "\n")))
          (insert "\n")

          ;; Commands
          (insert (propertize (format "Commands (%d):\n" (length cmds))
                              'face 'italic))
          (if (null cmds)
              (insert "  (none)\n")
            (dolist (cmd cmds)
              (insert "  ")
              (insert-button (symbol-name cmd)
                             'type 'help-function
                             'help-args (list cmd))

              ;; Add keybinding if available
              (when-let ((keys (where-is-internal cmd)))
                (insert (propertize 
                         (format "  %s" 
                                 (mapconcat #'key-description 
                                            (seq-take keys 2)  ; Show max 2 bindings
                                            ", "))
                         'face 'shadow)))

              ;; Add status
              (let ((status (rapid-package-inspector--symbol-status cmd)))
                (insert (propertize 
                         (format " [%s]" status)
                         'face 'shadow)))

              ;; Add docstring first line
              (when-let ((doc (documentation cmd t)))
                (insert (format "\n      %s" (rapid-package-inspector--first-line doc))))

              (insert "\n")))
          (insert "\n")

          ;; Hooks
          (insert (propertize (format "Hooks (%d):\n" (length hooks))
                              'face 'italic))
          (if (null hooks)
              (insert "  (none)\n")
            (dolist (hook hooks)
              (insert "  ")
              (insert-button (symbol-name hook)
                             'type 'help-variable
                             'help-args (list hook))

              ;; Add status
              (let ((status (rapid-package-inspector--symbol-status hook)))
                (unless (eq status 'loaded)
                  (insert (propertize 
                           (format " [%s]" status)
                           'face 'shadow))))

              ;; Add docstring first line
              (when-let ((doc (documentation-property 
                               hook 'variable-documentation t)))
                (insert (format "\n      %s" (rapid-package-inspector--first-line doc))))

              (insert "\n")))
          (insert "\n")

          ;; Modes and maps (if enabled)
          (when rapid-package-inspector-include-modes-and-maps
            (insert (propertize (format "Major modes (%d):\n" (length major))
                                'face 'italic))
            (if (null major)
                (insert "  (none)\n")
              (dolist (m major)
                (insert "  ")
                (insert-button (symbol-name m)
                               'type 'help-function
                               'help-args (list m))

                ;; Add status
                (let ((status (rapid-package-inspector--symbol-status m)))
                  (unless (eq status 'loaded)
                    (insert (propertize 
                             (format " [%s]" status)
                             'face 'shadow))))

                ;; Add docstring first line
                (when-let ((doc (documentation m t)))
                  (insert (format "\n      %s" (rapid-package-inspector--first-line doc))))

                (insert "\n")))
            (insert "\n")

            (insert (propertize (format "Minor modes (%d):\n" (length minor))
                                'face 'italic))
            (if (null minor)
                (insert "  (none)\n")
              (dolist (m minor)
                (insert "  ")
                (insert-button (symbol-name m)
                               'type 'help-function
                               'help-args (list m))

                ;; Add status
                (let ((status (rapid-package-inspector--symbol-status m)))
                  (unless (eq status 'loaded)
                    (insert (propertize 
                             (format " [%s]" status)
                             'face 'shadow))))

                ;; Add docstring first line
                (when-let ((doc (documentation m t)))
                  (insert (format "\n      %s" (rapid-package-inspector--first-line doc))))

                (insert "\n")))
            (insert "\n")

            (insert (propertize (format "Keymaps (%d):\n" (length maps))
                                'face 'italic))
            (if (null maps)
                (insert "  (none)\n")
              (dolist (km maps)
                (insert "  ")
                (insert-button (symbol-name km)
                               'type 'help-variable
                               'help-args (list km))

                ;; Add status
                (let ((status (rapid-package-inspector--symbol-status km)))
                  (unless (eq status 'loaded)
                    (insert (propertize 
                             (format " [%s]" status)
                             'face 'shadow))))

                ;; Add docstring first line
                (when-let ((doc (documentation-property 
                                 km 'variable-documentation t)))
                  (insert (format "\n      %s" (rapid-package-inspector--first-line doc))))

                (insert "\n")))
            (insert "\n")))))))

(provide 'rapid-package-inspector)

;;; rapid-package-inspector.el ends here
