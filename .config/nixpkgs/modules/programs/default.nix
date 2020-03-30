{ pkgs, config, ... }:
{
  imports = [
    ./ncmpcpp.nix
    ./onedrive.nix
  ];
}
