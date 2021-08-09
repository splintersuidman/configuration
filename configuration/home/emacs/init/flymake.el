(use-package flymake
  :ensure t)

(use-package flymake-diagnostic-at-point
  :ensure t
  :hook (flymake-mode . flymake-diagnostic-at-point-mode))

(provide 'init-flymake)
