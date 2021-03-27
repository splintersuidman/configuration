{ ... }: {
  programs.emacs.init = {
    prelude = ''
      (defvar after-load-theme-hook nil
        "Hook that is run after `load-theme' is called.")

      (defadvice load-theme (after run-after-load-theme-hook activate)
        "Run `after-load-theme-hook'."
        (run-hooks 'after-load-theme-hook))
    '';
    usePackage.base16-theme = {
      enable = true;
      after = [ "evil" ];
      init = ''
        (setq base16-distinct-fringe-background nil)
      '';
      config = ''
        (defun splinter-load-base16-theme (theme)
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
        (splinter-load-base16-theme 'base16-tomorrow-night)
      '';
      extraConfig = ''
        (defvar splinter-themes
          (list (lambda () (splinter-load-base16-theme 'base16-tomorrow))
                (lambda () (splinter-load-base16-theme 'base16-tomorrow-night)))
          "A list of functions that enable themes, that can be cycled
          through with `SPLINTER-SWITCH-THEME'.")

        (defun splinter-switch-theme ()
          "Switch to the next theme in `SPLINTER-THEMES'."
          (interactive)
          (let ((next (pop splinter-themes)))
            (setq splinter-themes (append splinter-themes (list next)))
            (funcall next)))
      '';
    };
  };
}
