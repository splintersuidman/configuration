(require 'init-keybindings)

(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-function helpful-macro helpful-command helpful-key helpful-variable helpful-at-point))

(use-package help-fns
  :after general
  :general
  (my-leader-def
    "h." 'display-local-help
    "h?" 'help-for-help
    "hC" 'describe-coding-system
    "hF" 'Info-goto-emacs-command-node
    "hI" 'describe-input-method
    "hK" 'Info-goto-emacs-key-command-node
    "hL" 'describe-language-environment
    "ha" 'apropos-command
    "hb" 'describe-bindings
    "hc" 'describe-key-briefly
    "hd" 'apropos-documentation
    "he" 'view-echo-area-messages
    "hf" 'helpful-callable
    "hg" 'describe-gnu-project
    "hh" 'view-hello-file
    "hi" 'info
    "hk" 'helpful-key
    "hl" 'view-lossage
    "hm" 'describe-mode
    "hn" 'view-emacs-news
    "ho" 'describe-symbol
    "hp" 'finder-by-keyword
    "hq" 'help-quit
    "hr" 'info-emacs-manual
    "hs" 'describe-syntax
    "ht" 'help-with-tutorial
    "hv" 'helpful-variable
    "hw" 'where-is))

(provide 'init-help)
