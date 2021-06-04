{ config, ... }: {
  services.mpdscribble = {
    enable = true;
    config = {
      host = "localhost";
      port = config.services.mopidy.config.mpd.port;
      verbose = 2;
      log = "syslog";
    };
  };
}
