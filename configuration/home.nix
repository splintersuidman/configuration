# Common home configuration that will be imported by the hosts.
{ config, ... }: {
  nixpkgs.config = import ./nixpkgs.nix;
}
