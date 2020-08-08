# Common system configuration that will be importer by the hosts.
{ ... }:
{
  nixpkgs.config = import ./nixpkgs.nix;
}
