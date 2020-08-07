# This module is only used to store the browser somewhere, so other modules can
# refer to it.
{ config, lib, pkgs, ... }:
let
  cfg = config.programs.browser;
in
{
  options = {
    programs.browser = {
      enable = lib.mkEnableOption "Whether to enable browser configuration.";

      package = lib.mkOption {
        type = lib.types.package;
        description = "Browser to use.";
      };

      program = lib.mkOption {
        type = lib.types.path;
        description = "Path to browser program.";
        example = "\${firefox}/bin/firefox";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];
  };
}
