{ pkgs, config, inputs, ... }:
let
  pass = "${config.programs.password-store.package}/bin/pass";
  passCommand = name: "${pass} ${name} | ${pkgs.coreutils}/bin/head -n 1";
in {
  home.packages = [ pkgs.mpc_cli pkgs.playerctl ];

  services.mopidy = {
    enable = false;
    extensionPackages = with pkgs.mopidyPackages; [
      (mopidy-mpd.overrideAttrs (oldAttrs: { src = inputs.mopidy-mpd; }))
      mopidy-mpris
      mopidy-spotify
    ];
    settings = {
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

  services.playerctld.enable = false;
}
