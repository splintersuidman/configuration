;;; -*- lexical-binding: t -*-

(require 'init-keybindings)

(use-package eglot
  :ensure t
  :demand t
  :after general
  :general
  (my-leader-def
    "la" '(eglot-code-actions :which-key "Code actions")
    "lb" '(flymake-show-buffer-diagnostics :which-key "Diagnostics")
    "lf" '(eglot-format :which-key "Format")
    "ll" '(eglot :which-key "Start LSP")
    "ln" '(flymake-goto-next-error :which-key "Next error")
    "lp" '(flymake-goto-prev-error :which-key "Previous error")
    "lq" '(eglot-shutdown :which-key "Shutdown LSP")
    "lQ" '(eglot-shutdown-all :which-key "Shutdown all LSP")
    "lr" '(eglot-rename :which-key "Rename")))

(use-package eldoc-box
  :ensure t
  :after eglot
  :custom
  (eldoc-box-only-multi-line t)
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))

(provide 'init-eglot)
