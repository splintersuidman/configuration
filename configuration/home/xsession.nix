{ pkgs, config, lib, ... }:
let
  wallpaper = "${config.xdg.userDirs.pictures}/wallpaper";

  xmonad-splintah = pkgs.xmonad-splintah;
  targetSystem = pkgs.stdenv.targetPlatform.system;
  # Rename executable to xmonad-${targetSystem} (e.g. xmonad-linux-x86_64) to
  # prevent XMonad from recompiling after boot.
  xmonad = pkgs.stdenv.mkDerivation {
    name = "xmonad";
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out/bin
      cp ${xmonad-splintah}/bin/xmonad-splintah $out/bin/xmonad
      cp ${xmonad-splintah}/bin/xmonad-splintah $out/bin/xmonad-${targetSystem}
    '';
  };
in {
  imports = [ ../../modules/home/wallpaper.nix ];

  home.packages = [ xmonad ];

  xsession = {
    enable = true;
    # windowManager.command = "${xmonad}/bin/xmonad-${targetSystem}";
    # profileExtra = ''
    #   export KDEWM=/home/splinter/.xmonad/xmonad-x86_64-linux
    # '';
  };

  systemd.user.sessionVariables = config.home.sessionVariables;

  wallpaper = {
    enableXSession = false;
    command =
      "${pkgs.feh}/bin/feh --no-fehbg --bg-fill --geometry +0+0 ${wallpaper}";
  };
}
