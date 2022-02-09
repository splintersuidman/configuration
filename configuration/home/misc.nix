{ pkgs, ... }:
let
  digital = pkgs.nur.repos.syberant.digital.overrideAttrs (old: {
    # Fix the fonts.
    installPhase = ''
      ${old.installPhase}
      makeWrapper $out/share/Digital/Digital/Digital.sh $out/bin/Digital \
        --set _JAVA_OPTIONS "-Dawt.useSystemAAFontSettings=lcd"
    '';
  });
in {
  home.packages = [
    pkgs.libreoffice
    pkgs.anki
    pkgs.wineWowPackages.stable
    pkgs.musescore
    digital
  ];
}
