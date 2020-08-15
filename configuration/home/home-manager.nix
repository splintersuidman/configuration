{ ... }:
let
  sources = import ../../nix/sources.nix;
in
{
  programs.home-manager = {
    enable = true;
    path = sources.home-manager.outPath;
  };
}
