;;; -*- lexical-binding: t -*-

(require 'init-eglot)
(require 'init-keybindings)

(use-package reformatter
  :ensure t)

(use-package purescript-mode
  :ensure t
  :after (general reformatter)
  :mode
  ("\\.purs\\'" . purescript-mode)
  ("\\.purs\\'" . purescript-mode)
  :config
  (defun splinter-purescript-hook ()
    (turn-on-purescript-indentation)
    (purescript-format-on-save-mode))
  (add-hook 'purescript-mode-hook #'splinter-purescript-hook)

  (reformatter-define purescript-format
    :program "purs-tidy"
    :args '("format")
    :group 'purescript-mode
    :lighter " Purs-Tidy")
  ;;;###autoload (autoload 'purescript-format-buffer "purescript-mode" nil t)
  ;;;###autoload (autoload 'purescript-format-region "purescript-mode" nil t)
  ;;;###autoload (autoload 'purescript-format-on-save-mode "purescript-mode" nil t)
  :general
  (my-local-leader-def
    :keymaps 'purescript-mode-map
    "F" 'purescript-format-region
    "f" 'purescript-format-buffer
    "s" 'purescript-pursuit
    "," 'purescript-mode-format-imports))

(provide 'init-purescript)
