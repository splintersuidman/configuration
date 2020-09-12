{ pkgs, ... }:
let
  # Discord has a habit of disallowing older client versions...
  discord =
    let
      # Nixpkgs for Discord v0.0.12.
      pkgs' = import (fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/977147bd04b8e48ab54e126b75a20c46c9afc6bb.tar.gz";
        sha256 = "1h0nfynn4cscvr0a695z0swmjgrglr1z7kp25jqhrpa9rsfyywza";
      }) { };
    in pkgs'.discord;
in
{
  home.packages = [
    pkgs.signal-desktop
    pkgs.qtox
    discord
  ];
}
