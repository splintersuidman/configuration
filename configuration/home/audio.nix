{ pkgs, ... }:
{
  home.packages = [
    pkgs.flac
    pkgs.cdparanoia
    pkgs.nur.repos.splintah.id3
  ];
}
