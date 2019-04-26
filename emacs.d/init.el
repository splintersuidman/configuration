(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Load the config file.
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
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
    (smartparens highlight-numbers flycheck-irony flycheck spaceline snippet company-irony company-iron tabbar expand-region moody neotree ghc ghc-mode awesome-tab toml-mode idris-mode proof-general company-coq rustic rust-mode switch-window rainbow-mode avy smex ido-vertical-mode pocket-reader nix-mode htmlize hackernews slime evil tuareg haskell-mode which-key use-package gruvbox-theme)))
 '(tabbar-mode nil nil (tabbar)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Iosevka"))))
 '(org-level-1 ((t (:foreground "#83a598"))))
 '(org-level-2 ((t (:foreground "#fabd2f"))))
 '(org-level-3 ((t (:foreground "#d3869b")))))
