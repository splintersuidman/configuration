{ pkgs, ... }:
{
  home.packages = [
    pkgs.signal-desktop
    pkgs.qtox
    pkgs.discord
  ];
}
