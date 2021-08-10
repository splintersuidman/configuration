{ pkgs, config, lib, ... }:
let
  fingerprints = {
    HDMI-A-0 =
      "00ffffffffffff0006102292ea07090203110103802b1b782ec601a3574a9d25125054000000b30001010101010101010101010101017c2e90a0601a1e4030203600b10e1100001a000000ff00324137303330434e55465a0a20000000fc0043696e656d610a202020202020000000000000000000000000000000000000015640010200000000762401a500ffff031a1aa8010000000000400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005f";
    eDP =
      "00ffffffffffff0006af3d4000000000211c0104a51f1178039b85925659902920505400000001010101010101010101010101010101143780b87038244010103e0035ae10000018000000000000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231343048414e30342e30200a0097";
    DisplayPort-1 =
      "00ffffffffffff0006102292ea07090203110103802b1b782ec601a3574a9d25125054000000b30001010101010101010101010101017c2e90a0601a1e4030203600b10e1100001a000000ff00324137303330434e55465a0a20000000fc0043696e656d610a202020202020000000000000000000000000000000000000015640010200000000762401a500ffff031a1aa8010000000000400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005f";
  };
in {
  programs.autorandr = {
    enable = true;
    profiles = {
      # Default single-display setup, with builtin laptop display as primary.
      default = {
        fingerprint = { inherit (fingerprints) eDP; };
        config = {
          eDP = {
            enable = true;
            primary = true;
            mode = "1920x1080";
            position = "0x0";
          };
        };
      };
      # Double-display setup with HDMI display as primary and builtin laptop
      # display as secondary.
      doublescreen-hdmi = {
        fingerprint = { inherit (fingerprints) HDMI-A-0 eDP; };
        config = {
          HDMI-A-0 = {
            enable = true;
            primary = true;
            mode = "1680x1050";
            position = "0x0";
          };
          eDP = {
            enable = true;
            mode = "1920x1080";
            position = "1680x0";
          };
        };
      };
      # Double-display setup with DisplayPort display as primary and builtin laptop
      # display as secondary.
      doublescreen-displayport = {
        fingerprint = { inherit (fingerprints) DisplayPort-1 eDP; };
        config = {
          DisplayPort-1 = {
            enable = true;
            primary = true;
            mode = "1680x1050";
            position = "0x0";
          };
          eDP = {
            enable = true;
            mode = "1920x1080";
            position = "1680x0";
          };
        };
      };
    };
    hooks = {
      postswitch = lib.optionalAttrs config.services.polybar.enable {
        "polybar-restart" =
          "${config.services.polybar.package}/bin/polybar-msg cmd restart";
      } // lib.optionalAttrs (config.wallpaper.command != null) {
        "wallpaper" = config.wallpaper.command;
      };
    };
  };

  # I use the NixOS option services.autorandr to run autorandr in batch mode (it
  # is necessary to run it as root), but it does not do it after boot.
  xsession.initExtra = ''
    ${pkgs.autorandr}/bin/autorandr --change
  '';
}
