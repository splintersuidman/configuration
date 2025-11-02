;;; -*- lexical-binding: t -*-

(require 'init-keybindings)
(require 'init-vterm)

(use-package reformatter
  :ensure t)

(use-package nushell-mode
  :ensure t
  :mode ("\\.nu\\'" . nushell-mode)
  :after (general vterm)
  :custom
  (vterm-shell "nu")
  :config
  (reformatter-define nushell-format
    :program "nufmt"
    :args '("--stdin")
    :group 'nushell-mode
    :lighter " Nufmt")
  ;;;###autoload (autoload 'nushell-format-buffer "nushell-mode" nil t)
  ;;;###autoload (autoload 'nushell-format-region "nushell-mode" nil t)
  ;;;###autoload (autoload 'nushell-format-on-save-mode "nushell-mode" nil t)
  (my-local-leader-def
    :keymaps 'nushell-mode-map
    "f" '(nushell-format-buffer :which-key "Format buffer")
    "F" '(nushell-format-region :which-key "Format region")))

(provide 'init-nushell)
