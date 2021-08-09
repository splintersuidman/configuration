(require 'init-keybindings)

(use-package scheme
  :custom
  (scheme-program-name "guile"))

(use-package geiser
  :disabled
  :ensure t
  :after (scheme company general)
  :custom
  (geiser-active-implementations '(guile))
  :general
  (my-local-leader-def
    :keymaps 'scheme-mode-map
    "\\" 'geiser-insert-lambda
    "b" 'geiser-eval-buffer
    "c" 'geiser-eval-definition
    "e" 'geiser-eval-last-sexp
    "k" 'geiser-compile-current-buffer
    "l" 'geiser-load-file
    "L" 'geiser-load-current-buffer
    "r" 'geiser-eval-region
    "s" 'geiser-set-scheme
    "z" 'geiser-doc-switch-to-repl))

;; TODO: geiser

(provide 'init-scheme)
