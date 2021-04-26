{ config, lib, ... }:
with lib;
let cfg = config.wallpaper;
in {
  options.wallpaper = {
    enableXSession = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to enable setting the wallpaper using X Session.
      '';
    };

    command = mkOption {
      type = types.str;
      default = null;
      description = ''
        Command to set wallpaper.
      '';
    };
  };

  config = mkIf cfg.enableXSession {
    xsession.initExtra = ''
      ${cfg.command}
    '';
  };
}
