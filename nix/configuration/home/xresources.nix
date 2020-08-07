{ pkgs, config, ... }:
let colours = config.colours; in
{
  xresources.properties = {
    "*background" = colours.background;
    "*foreground" = colours.foreground;
    "*color0" = colours.colour0;
    "*color8" = colours.colour8;
    "*color1" = colours.colour1;
    "*color9" = colours.colour9;
    "*color2" = colours.colour2;
    "*color10" = colours.colour10;
    "*color3" = colours.colour3;
    "*color11" = colours.colour11;
    "*color4" = colours.colour4;
    "*color12" = colours.colour12;
    "*color5" = colours.colour5;
    "*color13" = colours.colour13;
    "*color6" = colours.colour6;
    "*color14" = colours.colour14;
    "*color7" = colours.colour7;
    "*color15" = colours.colour15;
  };
}
