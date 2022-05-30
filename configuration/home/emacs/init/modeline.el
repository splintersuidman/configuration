(use-package simple
  :config
  (line-number-mode t)
  (column-number-mode t))

(use-package doom-modeline
  :ensure t
  :commands doom-modeline-mode
  :custom
  (doom-modeline-height 22)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon t)
  :init
  (doom-modeline-mode))

(provide 'init-modeline)
