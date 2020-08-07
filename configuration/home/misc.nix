{ pkgs, ... }:
{
  home.packages = [
    pkgs.libreoffice
    pkgs.anki
    pkgs.wineFull
    pkgs.musescore
  ];
}
