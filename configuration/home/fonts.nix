{ pkgs, ... }: {
  fonts.fontconfig.enable = true;

  imports = [
    ./fonts/iosevka.nix
  ];

  home.packages = with pkgs; [
    fantasque-sans-mono
    fira
    fira-code
    fira-mono
    font-awesome-ttf
    inconsolata
    libertine
    symbola
    dejavu_fonts
    siji
    unifont
    crimson
    ibm-plex
    mononoki

    fontforge-gtk
  ];
}
