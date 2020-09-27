{ ... }:
let
  sources = import ../../nix/sources.nix;
  unstable = import sources.nixpkgs-unstable { };
in {
  services.bitlbee = {
    enable = true;
    plugins = [ unstable.bitlbee-discord ];
  };
}
