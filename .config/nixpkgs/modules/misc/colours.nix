{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.colours;
in
{
  options = {
    colours =
      let
        colourOption = mkOption { type = types.str; };
        derivedColour = default: mkOption { type = types.str; inherit default; };
      in
      {
        black = colourOption;
        brightBlack = colourOption;
        red = colourOption;
        brightRed = colourOption;
        green = colourOption;
        brightGreen = colourOption;
        yellow = colourOption;
        brightYellow = colourOption;
        blue = colourOption;
        brightBlue = colourOption;
        magenta = colourOption;
        brightMagenta = colourOption;
        cyan = colourOption;
        brightCyan = colourOption;
        white = colourOption;
        brightWhite = colourOption;

        background = derivedColour cfg.black;
        foreground = derivedColour cfg.brightWhite;

        background0 = derivedColour cfg.background;
        background1 = derivedColour cfg.background;
        background2 = derivedColour cfg.background;
        background3 = derivedColour cfg.background;
        background4 = derivedColour cfg.background;
        foreground0 = derivedColour cfg.foreground;
        foreground1 = derivedColour cfg.foreground;
        foreground2 = derivedColour cfg.foreground;
        foreground3 = derivedColour cfg.foreground;
        foreground4 = derivedColour cfg.foreground;

        colour0 = derivedColour cfg.black;
        colour8 = derivedColour cfg.brightBlack;
        colour1 = derivedColour cfg.red;
        colour9 = derivedColour cfg.brightRed;
        colour2 = derivedColour cfg.green;
        colour10 = derivedColour cfg.brightGreen;
        colour3 = derivedColour cfg.yellow;
        colour11 = derivedColour cfg.brightYellow;
        colour4 = derivedColour cfg.blue;
        colour12 = derivedColour cfg.brightBlue;
        colour5 = derivedColour cfg.magenta;
        colour13 = derivedColour cfg.brightMagenta;
        colour6 = derivedColour cfg.cyan;
        colour14 = derivedColour cfg.brightCyan;
        colour7 = derivedColour cfg.white;
        colour15 = derivedColour cfg.brightWhite;
      };
  };
}
