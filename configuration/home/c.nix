{ pkgs, ... }:
{
  home.packages = [
    pkgs.gcc
    pkgs.binutils
    pkgs.indent
  ];

  programs.emacs.init.prelude = ''
    (setq c-basic-offset 4)
    (setq c-default-style "k&r")
  '';
}
