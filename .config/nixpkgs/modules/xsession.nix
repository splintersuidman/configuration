{ pkgs, config, ... }:
let
  wallpaper = "${config.home.homeDirectory}/wallpapers/A3MuFbJ.jpg";
in
{
  xsession = {
    enable = true;
    initExtra = ''
      ${pkgs.feh}/bin/feh --no-fehbg --bg-scale ${wallpaper}
    '';

    windowManager = {
      command = "${pkgs.haskellPackages.xmonad}/bin/xmonad";
    };
  };
}
