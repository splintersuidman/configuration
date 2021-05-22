{ pkgs, ... }: {
  home.packages = [ pkgs.elan pkgs.mathlibtools ];

  programs.emacs.init.usePackage.lean-mode = { enable = true; };
}
