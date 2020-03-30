{ pkgs, config, ... }:
{
  services.unclutter = {
    enable = true;
    timeout = 2;
  };
}
