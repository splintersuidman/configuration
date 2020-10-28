{ pkgs, config, ... }:
let
  nurSrc = (import ../../nix/sources.nix).nur;
  nurNoPkgs = import nurSrc { };
in
{
  imports = [
    nurNoPkgs.repos.splintah.hmModules.onedrive
  ];

  programs.onedrive = {
    enable = true;
    package = pkgs.onedrive;
    config = rec {
      sync_dir = "${config.home.homeDirectory}/.OneDrive";
      skip_dir = "~*|.~*|onbelangrijk";
      skip_file = skip_dir;
      upload_only = true;
    };
  };
}
