{ pkgs, ... }: {
  programs.emacs.init.usePackage = { dumb-jump = { enable = true; }; };
}
