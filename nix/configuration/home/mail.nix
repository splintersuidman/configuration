{ pkgs, ... }:
{
  home.packages = [
    pkgs.thunderbird
    pkgs.neomutt
  ];
}
