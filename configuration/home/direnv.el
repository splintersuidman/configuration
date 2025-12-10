;;; -*- lexical-binding: t -*-

(use-package direnv
  :ensure t
  :custom
  (direnv-always-show-summary nil)
  :config
  (direnv-mode))

(use-package direnv
  :after lsp-mode
  :hook
  (lsp-before-initialize . direnv-update-environment))

(provide 'init-direnv)
