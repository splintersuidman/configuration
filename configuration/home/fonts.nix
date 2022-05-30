{ pkgs, lib, ... }:
let
  charter = with pkgs;
    let version = "210112";
    in fetchzip rec {
      name = "charter-${version}";

      url = "https://practicaltypography.com/fonts/Charter%20${version}.zip";
      sha256 = "sha256-hBDXVRMTFGQ9WOCa1qnUcGpKs8y4waVKVlEta3fGaYI=";

      postFetch = ''
        mkdir -p $out/share/{doc,fonts}
        unzip -j $downloadedFile \*.ttf -d $out/share/fonts/truetype
        unzip -j $downloadedFile \*.otf -d $out/share/fonts/opentype
        unzip -j $downloadedFile \*license.txt -d $out/share/doc/${name}
      '';
    };
in {
  fonts.fontconfig.enable = true;

  imports = [ ./fonts/iosevka.nix ];

  home.packages = with pkgs; [
    charis-sil
    charter
    cooper-hewitt
    crimson
    dejavu_fonts
    eb-garamond
    fantasque-sans-mono
    fira
    fira-code
    fira-mono
    font-awesome
    gentium
    ibm-plex
    inconsolata
    libertine
    mononoki
    noto-fonts
    roboto
    roboto-mono
    roboto-slab
    siji
    source-code-pro
    source-sans
    source-serif
    symbola
    unifont

    fontforge-gtk
  ];
}
