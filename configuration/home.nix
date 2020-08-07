# Common home configuration that will be imported by the hosts.
{ ... }:
{
  nixpkgs.config = import ./nixpkgs.nix;
}
