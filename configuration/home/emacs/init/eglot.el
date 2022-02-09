(require 'init-keybindings)

(use-package eglot
  :ensure t
  :demand t
  :after general
  :general
  (my-leader-def
    "la" '(eglot-code-actions :which-key "Code actions")
    "lf" '(eglot-format :which-key "Format")
    "ll" '(eglot :which-key "Start LSP")
    "ln" '(flymake-goto-next-error :which-key "Next error")
    "lp" '(flymake-goto-prev-error :which-key "Previous error")
    "lq" '(eglot-shutdown :which-key "Shutdown LSP")
    "lQ" '(eglot-shutdown-all :which-key "Shutdown all LSP")
    "lr" '(eglot-rename :which-key "Rename")))

(provide 'init-eglot)
