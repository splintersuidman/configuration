;;; -*- lexical-binding: t -*-

(require 'init-keybindings)

(use-package nix-mode
  :ensure t
  :after general
  :mode "\\.nix\\'"
  :general
  (my-local-leader-def
    :keymaps 'nix-mode-map
    "c" '(nix-build :which-key "Build")
    "f" '(nix-format-buffer :which-key "Format buffer")
    "r" '(nix-repl :which-key "REPL")))

(use-package nix-mode
  :after eglot
  :init
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil"))))

(provide 'init-nix)
