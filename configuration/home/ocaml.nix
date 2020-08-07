{ config, ... }:
{
  programs.opam = {
    enable = true;
    enableBashIntegration = true;
  };

  home.sessionVariables = {
    OPAMROOT = "${config.xdg.dataHome}/opam";
  };
}
