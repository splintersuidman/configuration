(require 'init-eglot)
(require 'init-keybindings)

(use-package nix-mode
  :ensure t
  :after (general eglot)
  :mode "\\.nix\\'"
  :init
  (add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))
  :general
  (my-local-leader-def
    :keymaps 'nix-mode-map
    "c" '(nix-build :which-key "Build")
    "f" '(nix-format-buffer :which-key "Format buffer")
    "r" '(nix-repl :which-key "REPL")))

(provide 'init-nix)