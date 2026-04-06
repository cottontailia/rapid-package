;;; rapid-package-extensions.el --- User extensions for rapid-package -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cottontailia

;; Author: Cottontailia
;; Additional-Author: AI Assistant
;; URL: https://github.com/cottontailia/rapid-package
;; License: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Example user-land extensions for rapid-package.
;; Demonstrates use of the extension API:
;;   `rapid-package-add-keyword'
;;   `rapid-package-add-rewriter'
;;   `rapid-package-add-expander'
;;
;; Extensions provided:
;;   :straight              - install via straight.el
;;   :ensure-system-package - verify/install system packages
;;   :hydra                 - define hydras inline
;;
;; Note: This is a sample implementation for demonstration purposes.
;; It is not heavily maintained and may not reflect the latest
;; rapid-package.el features. For production use, consider adapting
;; these examples to your own needs.
;;
;; Load after rapid-package:
;;   (require 'rapid-package-extensions)

;;; Code:

(require 'rapid-package)

;;; :straight - straight.el integration

;; Values:
;;   :straight t           -> (straight-use-package 'PKG)
;;   :straight PKG         -> (straight-use-package 'PKG)
;;   :straight (PKG ...)   -> (straight-use-package '(PKG ...))
;;   :straight nil         -> manual management; :ensure is not removed
;;
;; When :straight has a non-nil value, :ensure is removed so that
;; package.el and straight.el do not both attempt installation.

(rapid-package-add-keyword :straight 'single 'package)

(rapid-package-add-rewriter :straight 'package
                            (lambda (plist _name)
                              (if (plist-get plist :straight)
                                  (map-delete plist :ensure)
                                plist)))

(rapid-package-add-expander :straight 'package
                            :position '(:before :install)
                            :expander (lambda (value name _plist)
                                        (cond
                                         ((eq value t)    `((straight-use-package ',name)))
                                         ((symbolp value) `((straight-use-package ',value)))
                                         ((listp value)   `((straight-use-package ',value)))
                                         (t (error ":straight value not recognized: %S" value)))))

;;; :ensure-system-package - system package verification

;; Values:
;;   :ensure-system-package CMD         -> check CMD, install CMD as package
;;   :ensure-system-package (CMD PKG)   -> check CMD, install PKG
;;   :ensure-system-package ((CMD1 PKG1) (CMD2 PKG2) ...)
;;
;; Checks are emitted as runtime calls to
;; `rapid-package--ensure-system-package' before the :install bucket.

(defun rapid-package--system-install-command ()
  "Return the system package manager install command as a string list.
Detects apt-get, brew, pacman, dnf, zypper in that order."
  (cond
   ((executable-find "apt-get") '("sudo" "apt-get" "install" "-y"))
   ((executable-find "brew")    '("brew" "install"))
   ((executable-find "pacman")  '("sudo" "pacman" "-S" "--noconfirm"))
   ((executable-find "dnf")     '("sudo" "dnf" "install" "-y"))
   ((executable-find "zypper")  '("sudo" "zypper" "install" "-y"))
   (t (user-error
       "rapid-package :ensure-system-package: no known package manager found"))))

(defun rapid-package--ensure-system-package (cmd pkg)
  "Ensure system command CMD is available; install PKG if not.
CMD and PKG are strings."
  (unless (executable-find cmd)
    (let* ((install-cmd (rapid-package--system-install-command))
           (full-cmd    (append install-cmd (list pkg))))
      (message "rapid-package: installing system package %s..." pkg)
      (unless (zerop (apply #'call-process (car full-cmd)
                            nil nil nil (cdr full-cmd)))
        (user-error "rapid-package: failed to install system package %s" pkg)))))

(defun rapid-package--ensure-system-package-entry (entry)
  "Return a `rapid-package--ensure-system-package' call form for ENTRY.
ENTRY may be a symbol, string, or a list (CMD PKG)."
  (cond
   ((symbolp entry)
    (let ((s (symbol-name entry)))
      `(rapid-package--ensure-system-package ,s ,s)))
   ((stringp entry)
    `(rapid-package--ensure-system-package ,entry ,entry))
   ;; List format: (CMD PKG)
   ((and (listp entry)
         (= (length entry) 2)
         (or (symbolp (car entry)) (stringp (car entry)))
         (or (symbolp (cadr entry)) (stringp (cadr entry))))
    (let ((cmd (let ((c (car entry))) (if (symbolp c) (symbol-name c) c)))
          (pkg (let ((p (cadr entry))) (if (symbolp p) (symbol-name p) p))))
      `(rapid-package--ensure-system-package ,cmd ,pkg)))
   (t
    (error ":ensure-system-package entry not recognized: %S" entry))))

(rapid-package-add-keyword :ensure-system-package 'single 'package)

(rapid-package-add-expander :ensure-system-package 'package
                            :position '(:before :install)
                            :expander (lambda (value _name _plist)
                                        (cond
                                         ;; single: symbol, string, or dotted pair
                                         ((or (symbolp value) (stringp value)
                                              (and (consp value) (not (listp (cdr value)))))
                                          (list (rapid-package--ensure-system-package-entry value)))
                                         ;; list of entries
                                         ((listp value)
                                          (mapcar #'rapid-package--ensure-system-package-entry value))
                                         (t
                                          (error ":ensure-system-package value not recognized: %S" value)))))

;;; :hydra - hydra.el integration

;; Generates defhydra forms from an inline declaration.
;; Any valid defhydra syntax works; the value is passed through as-is.
;;
;; Values:
;;   Single:   :hydra (NAME (BODY) DOCSTRING HEADS...)
;;   Multiple: :hydra ((NAME (BODY) ...) (NAME2 ...))
;;
;; Package macro: defhydra is placed in :config-post (after the package loads).
;; Conf macro:    defhydra is placed in :body.
;;
;; 'both cannot be used with a single :position here because valid bucket
;; names differ (:config-post is package-only, :body is conf-only).
;; The keyword is shared; expanders are registered separately.

(defun rapid-package--hydra-forms (value)
  "Return a list of defhydra forms from :hydra VALUE.
VALUE is either a single spec (NAME BODY DOC HEADS...) or a list of specs."
  (cond
   ((and (consp value) (symbolp (car value)))
    `((defhydra ,@value)))
   ((and (consp value) (consp (car value)))
    (mapcar (lambda (spec)
              (unless (and (consp spec) (symbolp (car spec)))
                (error ":hydra spec must start with a hydra name symbol: %S" spec))
              `(defhydra ,@spec))
            value))
   (t
    (error ":hydra value not recognized: %S" value))))

(rapid-package-add-keyword :hydra 'single 'both)

(rapid-package-add-expander :hydra 'package
                            :position :config-post
                            :expander (lambda (value _name _plist)
                                        (rapid-package--hydra-forms value)))

(rapid-package-add-expander :hydra 'conf
                            :position :body
                            :expander (lambda (value _name _plist)
                                        (rapid-package--hydra-forms value)))

(provide 'rapid-package-extensions)

;;; rapid-package-extensions.el ends here
