# rapid-package.el

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Emacs](https://img.shields.io/badge/Emacs-29.1+-purple.svg)](https://www.gnu.org/software/emacs/)

> 🚧 Work in Progress
>
> This project is currently under active development.
> Documentation is not ready yet.

## Fontset Example

```elisp
(rapid-package-fontset myfont
  :when-gui
  :variable
  (cjk-font   "Source Han Sans")   ; Adobe/Google pan-CJK font (han, kana, hangul)
  (emoji-font  "Noto Color Emoji") ; Google color emoji
  :base "Iosevka"
  :size 14
  :rules
  (han    ,cjk-font)
  (kana   ,cjk-font)
  (hangul ,cjk-font)
  (emoji  ,emoji-font)
  :rescale
  (,cjk-font  1.05)   ; widen CJK glyphs to match ASCII cell width
  (,emoji-font 0.90)  ; shrink emoji to avoid line-height expansion
  :default t
  :custom-face
  (fixed-pitch       ((t (:font ,myfont))))
  (fixed-pitch-serif ((t (:font ,myfont))))
  (variable-pitch    ((t (:font ,myfont)))))
```

## Example

```elisp
;;; init.el

(unless (package-installed-p 'rapid-package)
  (package-vc-install "https://github.com/cottontailia/rapid-package"))
(require 'rapid-package)
(rapid-package-load "~/.emacs.d/rapid-init.el")
```

```elisp
;;; rapid-init.el --- my settings -*- no-native-compile: t; -*-

;; global settings
(rapid-package-conf indent
  "Global indentation and whitespace behavior"
  :variable-default
  (tab-width 4 "Set default tab width to 4 columns")
  (indent-tabs-mode nil "Use spaces instead of tabs"))

;; package settings
(rapid-package vertico
  "Vertical interactive completion UI"
  :ensure t
  :mode-enable vertico-mode)

(rapid-package consult
  "Search and navigation commands"
  :ensure t
  :custom
  (consult-preview-key "M-." "Key to trigger live previews")
  (consult-line-start-from-top t "Start search from buffer top")
  :bind
  ("C-x b" . consult-buffer)
  ("M-g i" consult-imenu)
  ("M-y" consult-yank-from-kill-ring "Browse kill-ring with preview"))

(rapid-package orderless
  "Flexible completion matching style"
  :ensure t
  :custom
  (completion-styles (orderless basic) "Set completion matching styles"))

(rapid-package embark
  "Contextual actions menu"
  :ensure t :pin melpa
  :after vertico
  :with vertico-map
  (:bind ("C-." #'embark-act "Execute context-aware actions")))

(rapid-package treesit-env
  :ensure t
  :vc (:url "https://github.com/cottontailia/treesit-env")
  :custom
  (treesit-env-default-revision-auto t)
  (treesit-env-abi-max 14)
  :require treesit-env-recipe-placeholder
  :config
  (treesit-env-source treesit-env-recipe-placeholder)
  (treesit-env javascript json yaml))

(rapid-package-after treesit-env
  "Use Zig on Windows"
  :when-system windows-nt
  :env-path
  ,(expand-file-name "~/AppData/Local/Microsoft/WinGet/Links")
  :custom
  (treesit-env-compiler-cc '("zig" "cc" "-O3"))
  (treesit-env-compiler-c++ '("zig" "c++" "-O3")))

;; Local Variables:
;; eval: (use-local-map (copy-keymap (current-local-map)))
;; eval: (keymap-local-set "C-c i" #'rapid-package-inspector)
;; End:
```
