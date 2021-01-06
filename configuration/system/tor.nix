{ config, pkgs, ... }: {
  services.tor = {
    enable = true;
    client.enable = true;
  };
}
