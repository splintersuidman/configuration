;;; -*- lexical-binding: t -*-

(require 'init-keybindings)

(use-package ess
  :ensure t
  :custom
  (ess-use-ido nil)
  :general
  (my-local-leader-def
    :keymaps 'ess-r-mode-map
    "b" '(ess-eval-buffer :which-key "Eval buffer")
    "c" '(ess-eval-region-or-function-or-paragraph-and-step :which-key "Eval region, function or paragraph and step")
    "da" '(ess-display-help-apropos :which-key "Display help apropos")
    "dd" '(ess-display-help-on-object :which-key "Display help on object")
    "de" '(ess-describe-object-at-point :which-key "Describe object at point")
    "di" '(ess-display-package-index :which-key "Display package index")
    "ee" '(ess-execute :which-key "Execute")
    "f" '(ess-eval-function :which-key "Eval function")
    "j" '(ess-eval-line :which-key "Eval line")
    "l" '(ess-load-file :which-key "Load file")
    "n" '(ess-eval-line-visibly-and-step :which-key "Eval line and step")
    "p" '(ess-eval-paragraph-and-step :which-key "Eval paragraph and step")
    "q" '(ess-quit :which-key "Quit")
    "r" '(ess-eval-region :which-key "Eval region")))

(provide 'init-r)
