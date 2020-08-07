{ config, ... }:
{
  home.sessionVariables = {
    OCTAVE_HISTFILE = "${config.xdg.dataHome}/octave_hist";
  };
}
