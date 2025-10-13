;;; -*- lexical-binding: t -*-

(use-package all-the-icons
  :disabled
  :ensure t)

(use-package all-the-icons-dired
  :disabled
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(provide 'init-icons)
