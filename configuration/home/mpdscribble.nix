{ config, ... }: {
  services.mpdscribble = {
    enable = true;
    config = {
      mpdscribble = {
        host = "localhost";
        port = config.services.mopidy.config.mpd.port;
        verbose = 2;
        log = "syslog";
      };
    };
  };
}
