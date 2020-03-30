{ pkgs, config, ... }:
{
  services.redshift = {
    enable = true;
    provider = "manual";
    latitude = "52";
    longitude = "5";
  };
}
