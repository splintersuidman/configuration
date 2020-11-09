{ pkgs, config, ... }:
let
  wallpaper = "${config.xdg.userDirs.pictures}/A3MuFbJ.jpg";

  xmonad-splintah =
    pkgs.callPackage /home/splinter/.xmonad/xmonad-splintah/default.nix {
      nixpkgs = pkgs;
    };
  targetSystem = pkgs.stdenv.targetPlatform.system;
  # Rename executable to xmonad-${targetSystem} (e.g. xmonad-linux-x86_64) to
  # prevent XMonad from recompiling after boot.
  xmonad = pkgs.stdenv.mkDerivation {
    name = "xmonad";
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out/bin
      cp ${xmonad-splintah}/bin/xmonad-splintah $out/bin/xmonad-${targetSystem}
    '';
  };
in {
  xsession = {
    enable = true;
    initExtra = ''
      ${pkgs.feh}/bin/feh --no-fehbg --bg-scale ${wallpaper}
    '';

    windowManager.command = "${xmonad}/bin/xmonad-${targetSystem}";
  };
}
