{ ... }:
{
  programs.emacs.init = {
    usePackage.base16-theme = {
      enable = true;
      after = [ "evil" "general" ];
      init = ''
        (setq base16-distinct-fringe-background nil)
      '';

      config = ''
        (defun my/load-base16-theme (theme)
          "Load a base-16 theme. This function is used instead of
        just `(load-theme theme t)' to also set the colours of the evil
        cursors."
          ;; Set evil cursors.
          (let ((colors (intern (concat (symbol-name theme) "-colors"))))
            (setq evil-emacs-state-cursor   `(,(plist-get colors :base0D) box)
                  evil-insert-state-cursor  `(,(plist-get colors :base0D) bar)
                  evil-motion-state-cursor  `(,(plist-get colors :base0E) box)
                  evil-normal-state-cursor  `(,(plist-get colors :base0B) box)
                  evil-replace-state-cursor `(,(plist-get colors :base08) bar)
                  evil-visual-state-cursor  `(,(plist-get colors :base09) box)))
          (load-theme theme t))
        (my/load-base16-theme 'base16-tomorrow-night)

        ;; TODO: doesn't seem to work.
        (general-define-key
          :prefix my-leader
          :states '(normal visual motion)
          :keymaps 'override
          "tl" '(load-theme :which-key "Load theme")
          "tt" '(my/switch-theme :which-key "Switch theme"))
      '';
      extraConfig = ''
        (defvar my/themes
          (list (lambda () (my/load-base16-theme 'base16-tomorrow))
                (lambda () (my/load-base16-theme 'base16-tomorrow-night)))
          "A list of functions that enable themes, that can be cycled
          through with `MY/SWITCH-THEME'.")
  
        (defun my/switch-theme ()
          (interactive)
          (let ((next (pop my/themes)))
            (setq my/themes (append my/themes (list next)))
            (funcall next)))
      '';
    };
  };
}
