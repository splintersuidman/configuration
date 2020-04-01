(use-package haskell-mode
  :mode ("\\.hs\\'" "\\.lhs\\'")
  :init
  (setq haskell-stylish-on-save t)
  (setq haskell-tags-on-save nil)
  (setq haskell-process-suggest-remove-import-lines t)
  (setq haskell-process-log t)
  (setq haskell-process-type 'cabal-new-repl)
  (setq haskell-process-path-cabal "~/.local/bin/nix-cabal")

  (defun haskell-process-load-file-choose-type ()
    "Ask the user to choose a value for `haskell-process-type',
and load the current file using that process type."
    (interactive)
    (require 'ido)
    (let* ((process-types (mapcar 'cadr
                                  (cdr (custom-variable-type 'haskell-process-type))))
           (choices (mapcar 'symbol-name process-types))
           (choice (ido-completing-read "Haskell process type: " choices))
           (process-type (intern choice)))
      (let ((haskell-process-type process-type))
        (haskell-process-load-file))))

  :config
  (evil-leader/set-key-for-mode 'haskell-mode
    "c," 'haskell-mode-format-imports
    "cb" 'haskell-interactive-switch
    "cc" 'haskell-process-cabal-build
    "cef" 'haskell-goto-first-error
    "cen" 'haskell-goto-next-error
    "cep" 'haskell-goto-prev-error
    "cl" 'haskell-process-load-file
    "cL" 'haskell-process-load-file-choose-type
    "ck" 'haskell-interactive-mode-clear
    "cr" 'haskell-process-reload
    "cs" 'haskell-mode-stylish-buffer
    "ct" 'haskell-process-do-type
    "cv" 'haskell-cabal-visit-file
    "cx" 'haskell-process-cabal)

  (evil-leader/set-key-for-mode 'haskell-interactive-mode
    "cc" 'haskell-process-interrupt
    "cf" 'next-error-follow-minor-mode
    "ck" 'haskell-interactive-mode-clear
    "cn" 'haskell-interactive-mode-prompt-next
    "cp" 'haskell-interactive-mode-prompt-previous
    "cz" 'haskell-interactive-switch-back)

  :hook
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . haskell-auto-insert-module-template)
  (haskell-mode .
                (lambda ()
                  (set (make-local-variable 'company-backends)
                       (append '((company-capf company-dabbrev-code))
                               company-backends)))))
