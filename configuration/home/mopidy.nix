{ pkgs, config, inputs, ... }: {
  home.packages = [ pkgs.mpc_cli pkgs.playerctl ];

  services.mopidy = {
    enable = true;
    package = pkgs.mopidy;
    extraPackages = with pkgs.mopidyPackages; [
      (mopidy-mpd.overrideAttrs (oldAttrs: { src = inputs.mopidy-mpd; }))
      mopidy-mpris
      mopidy-spotify
    ];
    config = {
      mpd = {
        enabled = true;
        hostname = "::";
        port = 6600;
      };
      audio.mixer_volume = 30;
      spotify = {
        enabled = true;
        bitrate = 320;
        volume_normalization = true;
      };
      file = { enabled = false; };
      local = { enabled = false; };
    };
  };

  services.playerctld.enable = true;
}
