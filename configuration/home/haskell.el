(require 'init-eglot)
(require 'init-keybindings)

(use-package cus-edit
  :functions (custom-variable-type))

;; NOTE: haskell.el loads haskell-mode.el and other files such as
;; haskell-interactive-mode.el, but the package is called haskell-mode.
(use-package haskell
  :ensure haskell-mode
  :after (general eglot cus-edit)
  :mode
  ("\\.hs\\'" . haskell-mode)
  ("\\.hsc\\'" . haskell-mode)
  ("\\.c2hs\\'" . haskell-mode)
  ("\\.cpphs\\'" . haskell-mode)
  ("\\.lhs\\'" . haskell-literate-mode)
  :custom
  (haskell-stylish-on-save t)
  (haskell-tags-on-save nil)
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-log t)
  (haskell-process-type 'auto)
  :init
  (defun haskell-process-load-file-choose-type ()
    "Ask the user to choose a value for `haskell-process-type',
  and load the current file using that process type."
    (interactive)
    (let* ((process-types (mapcar 'cadr
                                  (cdr (custom-variable-type 'haskell-process-type))))
           (choices (mapcar 'symbol-name process-types))
           (choice (completing-read "Haskell process type: " choices))
           (process-type (intern choice)))
      (let ((haskell-process-type process-type))
        (haskell-process-load-file))))
  (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  :config
  (require 'haskell-doc)
  :hook
  (haskell-mode . interactive-haskell-mode)
  :general
  (my-local-leader-def
    :keymaps 'interactive-haskell-mode-map
    "," 'haskell-mode-format-imports
    "b" 'haskell-interactive-switch
    "c" 'haskell-process-cabal-build
    "ef" 'haskell-goto-first-error
    "en" 'haskell-goto-next-error
    "ep" 'haskell-goto-prev-error
    "f" 'haskell-mode-stylish-buffer
    "l" 'haskell-process-load-file
    "L" 'haskell-process-load-file-choose-type
    "k" 'haskell-interactive-mode-clear
    "r" 'haskell-process-reload
    "s" 'haskell-mode-stylish-buffer
    "t" 'haskell-process-do-type
    "v" 'haskell-cabal-visit-file
    "x" 'haskell-process-cabal)
  (my-local-leader-def
    :keymaps 'haskell-interactive-mode-map
    "c" 'haskell-process-interrupt
    "f" 'next-error-follow-minor-mode
    "k" 'haskell-interactive-mode-clear
    "n" 'haskell-interactive-mode-prompt-next
    "p" 'haskell-interactive-mode-prompt-previous
    "z" 'haskell-interactive-switch-back))

(provide 'init-haskell)
