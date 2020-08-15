# Common home configuration that will be imported by the hosts.
{ config, ... }:
let
  sources = import ../nix/sources.nix;
  nixpkgs = import sources.nixpkgs { inherit (config.nixpkgs) config; };
in
{
  _module.args.pkgs = nixpkgs;
  nixpkgs.config = import ./nixpkgs.nix;
}
