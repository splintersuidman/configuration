{ config, ... }:
{
  home.sessionVariables = {
    LESSHISTFILE = "${config.xdg.dataHome}/lesshst";
  };
}
