(use-package scheme
  :init
  (setq scheme-program-name "guile")

  (defun scheme-load-this-file ()
    (interactive)
    (save-buffer)
    (scheme-load-file (buffer-file-name)))

  ;; NOTE: replaced with `geiser'.
  ;; :config
  ;; (evil-leader/set-key-for-mode 'scheme-mode
  ;;   "ce" 'scheme-send-last-sexp
  ;;   "cl" 'scheme-load-file
  ;;   "cL" 'scheme-load-this-file
  ;;   "cr" 'run-scheme)
  )

(use-package geiser
  :after (scheme company)
  :init
  (setq geiser-active-implementations '(guile))
  :config
  (evil-leader/set-key-for-mode 'scheme-mode
    "c\\" 'geiser-insert-lambda
    "cb" 'geiser-eval-buffer
    "cc" 'geiser-eval-definition
    "ce" 'geiser-eval-last-sexp
    "ck" 'geiser-compile-current-buffer
    "cl" 'geiser-load-file
    "cL" 'geiser-load-current-buffer
    "cr" 'geiser-eval-region
    "cs" 'geiser-set-scheme
    "cz" 'geiser-doc-switch-to-repl))

