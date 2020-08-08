{ pkgs, config, ... }:
let
  colors = config.theme.base16.colors;
in
{
  xresources.properties = {
    "*background" = "#${colors.base00.hex.rgb}";
    "*foreground" = "#${colors.base05.hex.rgb}";
    "*color0" = "#${colors.base00.hex.rgb}";
    "*color8" = "#${colors.base03.hex.rgb}";
    "*color1" = "#${colors.base08.hex.rgb}";
    "*color9" = "#${colors.base08.hex.rgb}";
    "*color2" = "#${colors.base0B.hex.rgb}";
    "*color10" = "#${colors.base0B.hex.rgb}";
    "*color3" = "#${colors.base0A.hex.rgb}";
    "*color11" = "#${colors.base0A.hex.rgb}";
    "*color4" = "#${colors.base0D.hex.rgb}";
    "*color12" = "#${colors.base0D.hex.rgb}";
    "*color5" = "#${colors.base0E.hex.rgb}";
    "*color13" = "#${colors.base0E.hex.rgb}";
    "*color6" = "#${colors.base0C.hex.rgb}";
    "*color14" = "#${colors.base0C.hex.rgb}";
    "*color7" = "#${colors.base04.hex.rgb}";
    "*color15" = "#${colors.base05.hex.rgb}";
  };
}
