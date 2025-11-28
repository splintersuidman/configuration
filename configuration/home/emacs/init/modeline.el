;;; -*- lexical-binding: t -*-

(require 'init-icons)
(require 'init-tab-bar)

(use-package simple
  :config
  (line-number-mode t)
  (column-number-mode t))

(use-package doom-modeline
  :ensure t
  :after nerd-icons
  :commands doom-modeline-mode
  :custom
  (doom-modeline-height 22)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon t)
  (doom-modeline-workspace-name (not tab-bar-mode))
  :init
  (doom-modeline-mode))

(provide 'init-modeline)
