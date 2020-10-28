{ pkgs, config, ... }:
let
  nurSrc = (import ../../nix/sources.nix).nur;
  nurNoPkgs = import nurSrc { };

  getSecret = (import ../../nix/passenv.nix).lib.getSecret;
in {
  imports = [ nurNoPkgs.repos.splintah.hmModules.mopidy ];

  home.packages = [ pkgs.mpc_cli pkgs.playerctl ];

  services.mopidy = {
    enable = true;
    package = pkgs.mopidy;
    extraPackages = with pkgs.mopidyPackages; [
      mopidy-mpd
      mopidy-mpris
      mopidy-spotify
    ];
    config = {
      mpd = {
        enabled = true;
        hostname = "::";
        port = 6600;
      };
      spotify = {
        enabled = true;
        username = getSecret "SPOTIFY_USERNAME";
        password = getSecret "SPOTIFY_PASSWORD";
        client_id = getSecret "SPOTIFY_CLIENT_ID";
        client_secret = getSecret "SPOTIFY_CLIENT_SECRET";
        bitrate = 320;
        volume_normalization = true;
      };
      file = { enabled = false; };
      local = { enabled = false; };
    };
  };
}
