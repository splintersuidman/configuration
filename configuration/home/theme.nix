{ config, inputs, ... }:
let
  inherit (config.lib.theme.base16) fromYamlFile;

  kind = "dark";
  theme = let
    filename =
      if kind == "dark" then "tomorrow-night.yaml" else "tomorrow.yaml";
  in fromYamlFile "${inputs.base16-tomorrow-scheme}/${filename}";
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
