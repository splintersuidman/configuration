(require 'init-eglot)
(require 'init-keybindings)

(use-package rust-mode
  :ensure t
  :mode
  ("\\.rs\\'" . rust-mode)
  :after (general eglot)
  :init
  (add-to-list 'eglot-server-programs '(rust-mode . (eglot-rls "rls")))
  :general
  (my-local-leader-def
    :keymaps 'rust-mode-map
    "f" 'rust-format-buffer
    "F" 'rust-format-diff-buffer))

(provide 'init-rust)
