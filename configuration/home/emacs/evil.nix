{ ... }: {
  programs.emacs.init.usePackage = {
    # Note: should load undo-tree after evil; otherwise C-u scroll does not work
    # and evil-collection complains that evil-want-keybinding is not set to nil.
    undo-tree = {
      enable = true;
      after = [ "evil" ];
      config = ''
        (evil-set-undo-system 'undo-tree)
        (global-undo-tree-mode)
      '';
    };

    evil = {
      enable = true;
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

    # TODO: use a specialised function for leader-key definitions instead of
    # general-define-key. This function cannot be defined in the :config section
    # of general.el's use-package form, because then it will not be defined.
    general = {
      enable = true;
      after = [ "evil" ];
      init = ''
        (defconst my-leader "SPC"
          "Leader key.")
        (defconst my-local-leader (concat my-leader " c")
          "Local leader key.")

        (setq general-override-states '(insert emacs hybrid normal visual motion operator replace))
      '';
      config = ''
        (general-define-key
          :states '(normal visual motion)
          :prefix my-leader
          :keymaps 'override

          ;; Unbind leader key.
          "" nil

          ;; Prefix keys
          "b" '(:ignore t :which-key "Buffer")
          "c" '(:ignore t :which-key "Local")
          "e" '(:ignore t :which-key "Editing")
          "f" '(:ignore t :which-key "File")
          "g" '(:ignore t :which-key "Git")
          "h" '(:ignore t :which-key "Help")
          "j" '(:ignore t :which-key "Jump")
          "l" '(:ignore t :which-key "Lsp")
          "o" '(:ignore t :which-key "Org-mode")
          "t" '(:ignore t :which-key "Theme")
          "w" '(:ignore t :which-key "Window")
          "z" '(:ignore t :which-key "Org-roam")

          ;; Buffer-related commands under `b'
          ;; NOTE: counsel is nicer here, because of ivy-rich and preview. Same
          ;; for "bo".
          "bb" '(counsel-switch-buffer :which-key "Switch buffer")
          "bB" '(list-buffers :which-key "List buffers")
          "bd" '((lambda ()
                   (interactive)
                   (when (y-or-n-p "Kill this buffer? ")
                     (kill-this-buffer)))
                 :which-key "Kill this buffer")
          "bD" '(kill-this-buffer :which-key "Force kill this buffer")
          "bk" '(kill-buffer :which-key "Kill buffer")
          "bo" '(counsel-switch-buffer-other-window :which-key "Switch buffer other window")
          ;; Editing-related commands under `e'
          "es" '(evil-ex-sort :which-key "Sort")
          ;; File-related commands under `f'
          "fd" '(counsel-dired :which-key "Dired")
          "ff" '(counsel-find-file :which-key "Find file")
          "fr" '(my/counsel-rg :which-key "Ripgrep")
          "fs" '(save-buffer :which-key "Save buffer")
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
          "hf" 'counsel-describe-function
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
          "hv" 'counsel-describe-variable
          "hw" 'where-is
          ;; Window-related commands under `w'
          "wh" '(evil-window-left :which-key "Left")
          "wj" '(evil-window-down :which-key "Down")
          "wk" '(evil-window-up :which-key "Up")
          "wl" '(evil-window-right :which-key "Right")
          "wH" '(windmove-swap-states-left :which-key "Swap left")
          "wJ" '(windmove-swap-states-down :which-key "Swap down")
          "wK" '(windmove-swap-states-up :which-key "Swap up")
          "wL" '(windmove-swap-states-right :which-key "Swap right")
          "wo" '(other-window :which-key "Other window")
          "wv" '(split-window-horizontally :which-key "Split horizontally")
          "ws" '(split-window-vertically :which-key "Split vertically")
          "wd" '(delete-window :which-key "Delete")
          "w=" '(balance-windows :which-key "Balance")

          ;; M-x
          "SPC" '(counsel-M-x :which-key "M-x")
          "x" '(counsel-M-x :which-key "M-x")

          ;; Editing-related commands
          ";" '(comment-dwim :which-key "Comment")
          "+" '(evil-numbers/inc-at-pt :which-key "Increase at point")
          "-" '(evil-numbers/dec-at-pt :which-key "Decrease at point"))
      '';
    };
  };
}
