{ config, lib, ... }:
with lib;
let
  cfg = config.services.kmonad;

  keyboardOpts = { name, ... }: {
    options = {
      name = mkOption {
        type = types.str;
        readOnly = true;
        description = ''
          Unique name of the keyboard. This is set to the attribute name of the
          keyboard configuration.
        '';
      };

      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable the kmonad service for this keyboard.
        '';
      };

      device = mkOption {
        type = with types; either path str;
        description = ''
          The path to the device file.
        '';
      };

      config = mkOption {
        type = types.lines;
        default = "";
        description = ''
          The kmonad configuration for this keyboard.
        '';
      };
    };

    config = { name = mkDefault name; };
  };
in {
  options.services.kmonad = {
    keyboards = mkOption {
      type = types.attrsOf (types.submodule keyboardOpts);
      default = { };
      description = "List of keyboard configurations.";
    };

    package = mkOption {
      type = types.package;
      example = "pkgs.kmonad";
      description = ''
        The KMonad package.
      '';
    };
  };

  # NOTE: the top-level attrs assigned to config cannot be created by a fold or
  # merge over cfg.keyboards, it seems, as that results in an infinite
  # recursion.
  config =
    let enabledKeyboards = filterAttrs (name: kb: kb.enable) cfg.keyboards;
    in mkIf (cfg.keyboards != { }) {
      xdg.configFile = mapAttrs' (name: kb:
        nameValuePair "kmonad/kmonad-${name}.kbd" { text = kb.config; })
        enabledKeyboards;

      systemd.user.paths = mapAttrs' (name: kb:
        nameValuePair "kmonad-${name}" {
          Unit.Description = "Trigger for KMonad for ${name}";
          Install.WantedBy = [ "default.target" ];
          Path = {
            Unit = "kmonad-${name}.service";
            PathExists = kb.device;
          };
        }) enabledKeyboards;

      systemd.user.services = mapAttrs' (name: kb:
        nameValuePair "kmonad-${name}" {
          Unit.Description = "KMonad for ${name}";
          Service = {
            Type = "simple";
            ExecStart = "${cfg.package}/bin/kmonad ${
                config.xdg.configFile."kmonad/kmonad-${name}.kbd".source
              }";
          };
        }) enabledKeyboards;
    };
}
