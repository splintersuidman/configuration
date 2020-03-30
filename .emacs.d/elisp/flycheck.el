(use-package flycheck
  :hook
  (c-mode . flycheck-mode)
  (c++-mode . flycheck-mode))

(use-package flycheck-irony
  :disabled
  :after (irony flycheck)
  :hook (flycheck-mode . flycheck-irony-setup))
