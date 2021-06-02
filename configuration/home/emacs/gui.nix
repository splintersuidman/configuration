{ ... }: {
  programs.emacs.init.prelude = ''
    ;; Hide toolbar, scrollbar.
    (tool-bar-mode 0)
    (scroll-bar-mode 0)

    ;; Hide menubar on all systems but darwin.
    (when (not (eq system-type 'darwin))
      (menu-bar-mode 0))

    (setq inhibit-startup-screen t)

    ;; Wrap lines.
    (global-visual-line-mode t)

    ;; Use `y' and `n' instead of `yes' and `no'.
    (defalias 'yes-or-no-p 'y-or-n-p)

    ;; Inhibit messages for redefined functions.
    (setq ad-redefinition-action 'accept)
  '';

  programs.emacs.init.usePackage = {
    hl-line = {
      config = ''
        (global-hl-line-mode t)
      '';
    };

    hl-todo = {
      config = ''
        (global-hl-todo-mode t)
      '';
    };
  };
}
