# Common system configuration that will be importer by the hosts.
{ config, ... }:
let
  sources = import ../nix/sources.nix;
  nixpkgs = import sources.nixpkgs { inherit (config.nixpkgs) config; };
in {
  _module.args.pkgs = nixpkgs;
  nixpkgs.config = import ./nixpkgs.nix;
  nix.nixPath = [
    "nixpkgs=${sources.nixpkgs}"
    "nixos-config=/etc/nixos/configuration.nix"
  ];
}
