{ pkgs, config, ... }:
{
  services.picom = {
    enable = false;
    backend = "xrender";

    shadow = true;
    shadowOffsets = [ (-15) (-15) ];
    shadowOpacity = "0.25";
    shadowExclude = [
      "window_type = 'dock'"
      "window_type = 'desktop'"
      "window_type = 'dnd'"
      "window_type *= 'menu'"
      "window_type = 'utility'"
    ];
    extraOptions = ''
      shadow-radius = 60;
    '';
  };
}
