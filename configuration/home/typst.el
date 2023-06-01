(require 'init-keybindings)

(use-package typst-mode
  :ensure t
  :after (general eglot)
  :mode ("\\.typ\\'" . typst-mode)
  :init
  (add-to-list 'eglot-server-programs '(typst-mode . ("typst-lsp")))
  :general
  (my-local-leader-def
    :keymaps 'typst-mode-map
    "cc" 'typst-compile
    "cp" 'typst-preview
    "cw" 'typst-toggle-watch))

(provide 'init-typst)
