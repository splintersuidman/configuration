{ pkgs, config, ... }:
{
  imports = [
    ./mopidy.nix
    ./mpdscribble.nix
  ];
}
