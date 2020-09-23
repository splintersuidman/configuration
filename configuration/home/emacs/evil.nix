{ ... }:
{
  programs.emacs.init.usePackage = {
    evil = {
      enable = true;
      # after = [ "evil-leader" ];
      init = ''
        (setq evil-want-C-u-scroll t)
        (setq evil-want-keybinding nil)
        (setq evil-echo-state nil)
      '';
      config = ''
        (evil-mode)
      '';
    };

    evil-collection = {
      enable = true;
      after = [ "evil" ];
      init = ''
        (setq evil-collection-setup-minibuffer nil)
      '';
      config = ''
        (evil-collection-init)
      '';
    };

    evil-numbers = {
      enable = true;
      command = [ "evil-numbers/inc-at-pt" "evil-numbers/dec-at-pt" ];
    };

    evil-surround = {
      enable = true;
      config = ''
        (global-evil-surround-mode)
      '';
    };

    evil-leader = {
      enable = true;
      after = [ "evil" ];
      init = ''
        (setq evil-leader/in-all-states t)
      '';
      # TODO: does not seem to work.
      extraConfig = ''
        :functions
        (evil-leader/set-key
         eval-leader/set-leader
         global-evil-leader-mode)
      '';
      config = ''
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
          ;; Editing-related commands under `e'
          "es" 'evil-ex-sort
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

          ;; Editing-related commands
          ";" 'comment-line
          "+" 'evil-numbers/inc-at-pt
          "-" 'evil-numbers/dec-at-pt)
        (global-evil-leader-mode t)
      '';
    };
  };
}
