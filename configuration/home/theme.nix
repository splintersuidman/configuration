{ config, ... }:
let
  inherit (config.lib.theme.base16) fromYamlFile;

  sources = import ../../nix/sources.nix;
  nurNoPkgs = import sources.nur { };

  kind = "dark";
  theme = let
    filename =
      if kind == "dark" then "tomorrow-night.yaml" else "tomorrow.yaml";
  in fromYamlFile "${sources.base16-tomorrow-scheme}/${filename}";
in {
  imports = [ nurNoPkgs.repos.rycee.hmModules.theme-base16 ];

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
