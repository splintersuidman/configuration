{ config, pkgs, ... }:
{
  programs.password-store = {
    enable = true;
    settings = {
      PASSWORD_STORE_DIR = config.xdg.dataHome + "/password-store";
    };
  };
  programs.browserpass.enable = true;
}
