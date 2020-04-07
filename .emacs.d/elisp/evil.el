(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq evil-echo-state nil)

  :config
  (use-package evil-collection
    :after evil
    :init
    (setq evil-collection-setup-minibuffer nil)
    :config
    (evil-collection-init))

  ;; Add commands `evil-numbers/inc-at-pt' and
  ;; `evil-numbers/dec-at-pt' to increment and decrement the number at
  ;; point.
  (use-package evil-numbers
    :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

  ;; Add key bindings like `dsb' (delete text between brackets).
  (use-package evil-surround
    :config
    (global-evil-surround-mode))

  (use-package evil-leader
    :init
    (setq evil-leader/in-all-states t)
    :config
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      ;; Buffer-related commands under `b'
      "bb" 'switch-to-buffer
      "bB" 'list-buffers
      "bd" '(lambda ()
              (interactive)
              (when (y-or-n-p "Kill this buffer? ")
                (kill-this-buffer)))
      "bD" 'kill-this-buffer
      "bk" 'kill-buffer
      ;; Language-specific commands under `c'
      "cc" 'compile
      ;; "cm" 'make
      ;; File-related commands under `f'
      "fd" 'dired
      "ff" 'find-file
      "fs" 'save-buffer
      ;; Help-related commands under `h'
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
      "hf" 'describe-function
      "hg" 'describe-gnu-project
      "hh" 'view-hello-file
      "hi" 'info
      "hk" 'describe-key
      "hl" 'view-lossage
      "hm" 'describe-mode
      "hn" 'view-emacs-news
      "ho" 'describe-symbol
      "hp" 'finder-by-keyword
      "hq" 'help-quit
      "hr" 'info-emacs-manual
      "hs" 'describe-syntax
      "ht" 'help-with-tutorial
      "hv" 'describe-variable
      "hw" 'where-is
      ;; Theme-related commands under `t'
      "tt" 'my/switch-theme
      ;; Window-related commands under `w'
      "wh" 'evil-window-left
      "wj" 'evil-window-down
      "wk" 'evil-window-up
      "wl" 'evil-window-right
      "wo" 'other-window
      "wv" 'split-window-horizontally
      "ws" 'split-window-vertically
      "wd" 'delete-window
      "w=" 'balance-windows
      ;; Zetteldeft-related commands
      "zd" 'deft
      "zD" 'zetteldeft-deft-new-search
      "zR" 'deft-refresh
      "zs" 'zetteldeft-search-at-point
      "zc" 'zetteldeft-search-current-id
      "zf" 'zetteldeft-follow-link
      "zF" 'zetteldeft-avy-file-search-ace-window
      "zl" 'zetteldeft-avy-link-search
      "zt" 'zetteldeft-avy-tag-search
      "zT" 'zetteldeft-tag-buffer
      "zi" 'zetteldeft-find-file-id-insert
      "zI" 'zetteldeft-find-file-full-title-insert
      "zo" 'zetteldeft-find-file
      "zn" 'zetteldeft-new-file
      "zN" 'zetteldeft-new-file-and-link
      "zr" 'zetteldeft-file-rename
      "zx" 'zetteldeft-count-words

      ;; Editing-related commands
      ";" 'comment-line
      "+" 'evil-numbers/inc-at-pt
      "-" 'evil-numbers/dec-at-pt)
    (global-evil-leader-mode t))

  (evil-mode))
