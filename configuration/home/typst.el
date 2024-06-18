(require 'init-keybindings)

(use-package typst-mode
  :disabled
  :ensure t
  :after (general eglot)
  :mode ("\\.typ\\'" . typst-mode)
  :init
  (add-to-list 'eglot-server-programs '(typst-mode . ("typst-lsp")))
  :general
  (my-local-leader-def
    :keymaps 'typst-mode-map
    "c" 'typst-compile
    "p" 'typst-preview
    "w" 'typst-toggle-watch))

;; (use-package typst-ts-mode
;;   :demand t
;;   :after (general eglot)
;;   :mode ("\\.typ\\'" . typst-ts-mode)
;;   :custom
;;   (typst-ts-mode-indent-offset 2)
;;   :init
;;   (add-to-list 'eglot-server-programs '(typst-mode . ("typst-lsp")))
;;   :general
;;   (my-local-leader-def
;;     :keymaps 'typst-mode-map
;;     "c" 'typst-ts-mode-compile
;;     "p" 'typst-ts-mode-preview
;;     "w" 'typst-ts-mode-watch-toggle))

(provide 'init-typst)
