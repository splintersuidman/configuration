{ pkgs, config, ... }:
let
  wallpaper = "${config.xdg.userDirs.pictures}/A3MuFbJ.jpg";
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
