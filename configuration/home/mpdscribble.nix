{ pkgs, config, ... }:
let
  nurSrc = (import ../../nix/sources.nix).nur;
  nurNoPkgs = import nurSrc { };

  getSecret = (import ../../nix/passenv.nix).lib.getSecret;
in
{
  imports = [
    nurNoPkgs.repos.splintah.hmModules.mpdscribble
  ];

  services.mpdscribble = {
    enable = true;
    config = {
      mpdscribble = {
        host = "localhost";
        port = config.services.mopidy.config.mpd.port;
        verbose = 2;
        log = "syslog";
      };
      "libre.fm" = {
        url = "https://turtle.libre.fm/";
        username = getSecret "LIBREFM_USERNAME";
        password = getSecret "LIBREFM_PASSWORD";
        journal = "${config.xdg.dataHome}/mpdscribble/librefm.journal";
      };
      "listenbrainz" = {
        url = "http://proxy.listenbrainz.org";
        username = getSecret "LISTENBRAINZ_USERNAME";
        password = getSecret "LISTENBRAINZ_AUTH_TOKEN";
        journal = "${config.xdg.dataHome}/mpdscribble/listenbrainz.journal";
      };
    };
  };
}
