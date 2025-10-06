;;; -*- lexical-binding: t -*-

(require 'init-eglot)
(require 'init-keybindings)

(use-package rust-mode
  :ensure t
  :mode
  ("\\.rs\\'" . rust-ts-mode)
  :after (general eglot)
  :init
  (add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode) . ("rust-analyzer")))
  :general
  (my-local-leader-def
    :keymaps 'rust-ts-mode-map
    "f" 'rust-format-buffer
    "F" 'rust-format-diff-buffer))

(provide 'init-rust)
