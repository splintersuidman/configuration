{ pkgs, config, ... }:
{
  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 300;
    maxCacheTtl = 999999;
    enableSshSupport = true;
  };
}
