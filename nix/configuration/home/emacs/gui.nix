{ ... }:
{
  programs.emacs.init.prelude = ''
    ;; Hide toolbar, scrollbar.
    (tool-bar-mode 0)
    (scroll-bar-mode 0)

    ;; Hide menubar on all systems but darwin.
    (when (not (eq system-type 'darwin))
      (menu-bar-mode 0))

    ;; Show line number and column number in modeline.
    (line-number-mode t)
    (column-number-mode t)

    ;; Highlight the cursorline.
    (when window-system
      (global-hl-line-mode t))

    ;; Don't show splash screen.
    (setq inhibit-splash-screen t)

    ;; Wrap lines.
    (global-visual-line-mode t)

    ;; Use `y' and `n' instead of `yes' and `no'.
    (defalias 'yes-or-no-p 'y-or-n-p)
  '';
}
