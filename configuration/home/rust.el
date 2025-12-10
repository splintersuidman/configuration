;;; -*- lexical-binding: t -*-

(require 'init-keybindings)

(use-package rust-mode
  :ensure t
  :mode
  ("\\.rs\\'" . rust-ts-mode)
  :after general
  :general
  (my-local-leader-def
    :keymaps 'rust-ts-mode-map
    "f" 'rust-format-buffer
    "F" 'rust-format-diff-buffer))

(use-package rust-mode
  :after eglot
  :init
  (add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode) . ("rust-analyzer"))))

(use-package lsp-rust
  :after (lsp-mode rust-mode)
  :hook
  (rust-mode . lsp)
  (rust-ts-mode . lsp))

(provide 'init-rust)
