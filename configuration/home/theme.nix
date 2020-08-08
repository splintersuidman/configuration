{ config, ... }:
let
  inherit (config.lib.theme.base16) fromYamlFile;

  sources = import ../../nix/sources.nix;
  nurNoPkgs = import sources.nur { };
in
{
  imports = [
    nurNoPkgs.repos.rycee.hmModules.theme-base16
  ];

  theme.base16 = rec {
    kind = "dark";
    colors =
      let
        file = if kind == "dark"
          then "tomorrow-night.yaml"
          else "tomorrow.yaml";
      in
        fromYamlFile "${sources.base16-tomorrow-scheme}/${file}";
  };
}
