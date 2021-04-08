{ ... }: {
  programs.emacs.init.prelude = ''
    (setq backup-directory-alist '(("." . "~/.cache/emacs/backup")))
  '';
}
