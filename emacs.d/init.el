(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; Load the config file.
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" default)))
 '(electric-pair-mode t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-path-ghci "/run/current-system/sw/bin/ghci")
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-tags-on-save t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(line-number-mode t)
 '(package-selected-packages
   (quote
    (magit evil-collection evil-surround hindent yaml-mode shm ox-odt word-count word-count-mode intero smartparens highlight-numbers flycheck-irony flycheck spaceline snippet company-irony company-iron expand-region moody neotree ghc ghc-mode awesome-tab toml-mode idris-mode proof-general company-coq rustic rust-mode switch-window rainbow-mode avy smex ido-vertical-mode pocket-reader nix-mode htmlize hackernews slime evil tuareg haskell-mode which-key use-package gruvbox-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Iosevka"))))
 '(org-level-1 ((t (:foreground "#83a598"))))
 '(org-level-2 ((t (:foreground "#fabd2f"))))
 '(org-level-3 ((t (:foreground "#d3869b")))))
