# Adapted from
# https://github.com/eikek/confnix/blob/nixos-20.03/pkgs/mpdscribble/module.nix,
# which is unlicensed.

{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.services.mpdscribble;
  mkAccount = name: val: ''
    [${name}]
    url = ${val.url}
    username = ${val.username}
    password = ${val.password}
    journal = ${cfg.baseDir}/${name}.journal
  '';
  configFile = pkgs.writeText "mpdscribble.conf" ''
    [mpdscribble]
    host = ${cfg.mpdHost}
    port = ${toString cfg.mpdPort}
    log = syslog
    verbose = 2
   ${builtins.concatStringsSep "\n\n" (builtins.attrValues (builtins.mapAttrs mkAccount cfg.accounts))}
  '';
in {

  ## interface
  options = {
    services.mpdscribble = {
      enable = mkOption {
        default = false;
        description = "Whether to enable mpdscribble.";
      };

      mpdHost = mkOption {
        type = types.str;
        default = "localhost";
        description = "The host to reach mpd.";
      };

      mpdPort = mkOption {
        type = types.int;
        default = 6600;
        description = "The port used to bind the http rest server.";
      };

      baseDir = mkOption {
        default = "/var/data/mpdscribble";
        description = "Location where mpdscribble puts its journal.";
      };

      accounts = mkOption {
        type = types.attrsOf (types.submodule {
          options = {
            username = mkOption {
              type = types.str;
              example = "johndoe";
              description = "The username for the libre.fm account";
            };
            password = mkOption {
              type = types.str;
              example = "x123";
              description = "The password to the librefm account";
            };
            url = mkOption {
              type = types.str;
              default = "http://turtle.libre.fm";
              description = "The scrobbler url of libre.fm";
            };
          };
        });
      };
    };
  };

  ## implementation
  config = mkIf config.services.mpdscribble.enable {
    users.extraGroups = singleton {
      name = "mpdscribble";
      # gid = config.ids.gids.mpdscribble;
    };

    users.extraUsers = singleton {
      name = "mpdscribble";
      # uid = config.ids.uids.mpdscribble;
      extraGroups = [ "mpdscribble" ];
      description = "Mpdscribble daemon user.";
    };

    systemd.services.mpdscribble = {
      description = "Mpdscribble server";
      after = [ "networking.target" ];
      wantedBy = [ "multi-user.target" ];

      preStart = ''
        mkdir -p ${cfg.baseDir}
        chown mpdscribble:mpdscribble ${cfg.baseDir}
      '';

      script = "${pkgs.su}/bin/su -s ${pkgs.bash}/bin/sh mpdscribble -c \"${pkgs.mpdscribble}/bin/mpdscribble --conf ${configFile} --no-daemon \"";
    };
  };
}
