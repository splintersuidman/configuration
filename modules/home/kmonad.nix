{ config, lib, ... }:
let cfg = config.services.kmonad;
in with lib; {
  options.services.kmonad = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to enable the kmonad service.
      '';
    };

    config = mkOption {
      type = types.lines;
      default = "";
      description = ''
        The kmonad configuration.
      '';
    };

    package = mkOption {
      type = types.package;
      example = "pkgs.kmonad";
      description = ''
        The kmonad package.
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    xdg.configFile."kmonad/kmonad.kbd".text = cfg.config;

    systemd.user.services.kmonad = {
      Unit = { Description = "kmonad"; };
      Install = { WantedBy = [ "graphical.target" ]; };
      Service = {
        Type = "simple";
        ExecStart = "${cfg.package}/bin/kmonad ${
            config.xdg.configFile."kmonad/kmonad.kbd".source
          }";
      };
    };
  };
}
