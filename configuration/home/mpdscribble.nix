{ config, ... }: {
  services.mpdscribble = {
    enable = false;
    # config = {
    #   host = "localhost";
    #   port = config.services.mopidy.config.mpd.port;
    #   verbose = 2;
    #   log = "syslog";
    # };
  };
}
