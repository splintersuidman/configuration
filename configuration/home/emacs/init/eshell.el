(require 'init-keybindings)

(use-package eshell
  :after general
  :general
  (my-local-leader-def
   :keymaps 'eshell-mode-map
   "h" 'counsel-esh-history)
  :config
  (defun eshell-new ()
    "Create a new eshell."
    ;; The argument to `eshell' does not really matter, as long as it's
    ;; not an integer (n), which causes Emacs to open the nth `eshell'.
    (eshell 'new)))

(provide 'init-eshell)
