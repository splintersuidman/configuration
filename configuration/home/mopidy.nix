# TODO: update to new mopidy; this requires some changes in
# pkgs.nur.repos.splintah.hmModules.mopidy, because mopidy switched to
# Python 3 and MPD server support was moved to an external package,
# mopidy-mpd, which is not in nixos-20.03.
# See <https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/audio/mopidy.nix>.
{ config, ... }:
let
  nurSrc = (import ../../nix/sources.nix).nur;
  nurNoPkgs = import nurSrc { };

  getSecret = (import ../../nix/passenv.nix).lib.getSecret;

  # See <https://lazamar.co.uk/nix-versions/?package=mopidy>.
  pkgs = import (builtins.fetchGit {
    name = "nixpkgs-unstable-mopidy-2.3.1";
    url = "https://github.com/nixos/nixpkgs-channels/";
    ref = "refs/heads/nixpkgs-unstable";
    rev = "2158ec610d90359df7425e27298873a817b4c9dd";
  }) { };

  mopidy-mpris = with pkgs; pythonPackages.buildPythonApplication rec {
    pname = "Mopidy-MPRIS";
    version = "2.0.0";
    src = fetchFromGitHub {
      owner = "mopidy";
      repo = "mopidy-mpris";
      rev = "v${version}";
      sha256 = "0vm4whch26aalg3yk57fsbjx5smjxn2vqpqkk8i13mbn1b8vv7yl";
    };

    propagatedBuildInputs = [
      mopidy
      pythonPackages.pydbus
    ];

    doCheck = false;

    meta = with stdenv.lib; {
      homepage = https://github.com/mopidy/mopidy-mpris;
      description = "Mopidy extension for controlling Mopidy through D-Bus using the MPRIS specification.";
      license = licenses.asl20;
    };
  };
in
{
  imports = [
    nurNoPkgs.repos.splintah.hmModules.mopidy
  ];

  home.packages = [
    pkgs.mpc_cli
    pkgs.playerctl
  ];

  services.mopidy = {
    enable = true;
    package = pkgs.mopidy;
    extraPackages = with pkgs; [ mopidy-spotify mopidy-mpris ];
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
      file = {
        enabled = false;
      };
      local = {
        enabled = false;
      };
    };
  };
}
