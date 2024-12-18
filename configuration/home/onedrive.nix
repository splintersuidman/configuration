{ pkgs, config, ... }:
let onedrive = pkgs.unstable.onedrive; in
{
  programs.onedrive = {
    enable = true;
    package = onedrive;
    config = rec {
      sync_dir = "${config.home.homeDirectory}/.OneDrive";
      monitor_interval = 10 * 60; # 10 minutes
      disable_notifications = "true";
      check_nosync = "true";
    };
  };

  xdg.configFile."onedrive/sync_list".text = ''
    !*.aux
    !*.bbl
    !*.bcf
    !*.blg
    !*.blob
    !*.part
    !*.run.xml
    !.ccls-cache/*
    !.git/*
    !.mypy_cache/*
    !__pycache__/*
    !#*
    /Universiteit/
  '';

  systemd.user.services.onedrive = {
    Unit = {
      Description = "OneDrive";
      After = [ "network-online.target" ];
      Wants = [ "network-online.target" ];
    };
    Service = {
      ProtectSystem = "full";
      ProtectHostname = true;
      ProtectKernelTunables = true;
      ProtectControlGroups = true;
      RestrictRealtime = true;
      ExecStart = "${onedrive}/bin/onedrive --monitor";
      Restart = "on-failure";
      RestartSec = 3;
      RestartPreventExitStatus = 3;
    };
    Install.WantedBy = [ "default.target" ];
  };
}
