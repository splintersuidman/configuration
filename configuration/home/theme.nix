{ config, inputs, ... }:
let
  inherit (config.lib.theme.base16) fromYamlFile;

  hexColour = with builtins;
    hex: {
      hex = {
        r = substring 1 2 hex;
        g = substring 3 2 hex;
        b = substring 5 2 hex;
      };
    };

  kind = "dark";
  tomorrow = let
    filename =
      if kind == "dark" then "tomorrow-night.yaml" else "tomorrow.yaml";
  in fromYamlFile "${inputs.base16-tomorrow-scheme}/${filename}";

  modus-vivendi = {
    name = "Modus Vivendi";
    colors = {
      base00 = hexColour "#000000";
      base01 = hexColour "#100f10";
      base02 = hexColour "#323232";
      base03 = hexColour "#505050";
      base04 = hexColour "#e0e6f0";
      base05 = hexColour "#ffffff";
      base06 = hexColour "#e0e6f0";
      base07 = hexColour "#ffffff";
      base08 = hexColour "#ff8059";
      base09 = hexColour "#ef8b50";
      base0A = hexColour "#d0bc00";
      base0B = hexColour "#44bc44";
      base0C = hexColour "#00d3d0";
      base0D = hexColour "#2fafff";
      base0E = hexColour "#feacd0";
      base0F = hexColour "#8b1030";
    };
  };

  theme = modus-vivendi;

  colors = config.theme.base16.colors;
in {
  theme.base16 = rec {
    inherit kind;
    inherit (theme) name colors;
  };

  # These options default to `true`, but they do and install some things I don't
  # want, so I disable them.
  gtk.enableBase16Theme = false;
  programs.bat.enableBase16Theme = false;
  programs.gnome-terminal.enableBase16Theme = false;
  programs.rofi.enableBase16Theme = false;
  services.dunst.enableBase16Theme = false;
  services.polybar.enableBase16Theme = false;
  services.xscreensaver.enableBase16Theme = false;
}
