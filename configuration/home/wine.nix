{ config, ... }:
{
  home.sessionVariables = {
    WINEPREFIX = "${config.xdg.dataHome}/wine";
  };
}
