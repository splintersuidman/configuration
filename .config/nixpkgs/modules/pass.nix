{ config, pkgs, ... }:
{
  programs.browserpass.enable = true;
  home.packages = [ pkgs.pass ];
  home.sessionVariables = {
    PASSWORD_STORE_DIR = config.xdg.dataHome + "/password-store";
  };
}
