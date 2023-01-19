{ config, pkgs, ... }: {
  services.redshift = {
    enable = false;
    provider = "manual";
    latitude = "52";
    longitude = "5";
  };
}
