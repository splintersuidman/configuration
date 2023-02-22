(use-package desktop
  :disabled
  :custom
  (desktop-dirname "~/.emacs.d/")
  (desktop-auto-save-timeout (* 2 60))
  :config
  (desktop-save-mode))

(provide 'init-desktop)
