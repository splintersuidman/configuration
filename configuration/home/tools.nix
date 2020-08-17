{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # Files
    ranger
    transmission-gtk
    curl
    wget

    # Images
    sxiv
    scrot
    gimp
    inkscape

    # Programming tools
    gnumake

    # Man pages
    manpages
    # stdmanpages # C++ std documentation manpages

    # Type setting
    pandoc
    groff

    # Video
    vlc
    youtubeDL
    ffmpeg

    # Other/system tools
    aspell aspellDicts.en aspellDicts.nl
    calc
    cmatrix
    coreutils
    dmenu
    entr
    fd
    file
    gnupg
    gnutar
    gtypist
    hplip
    intel-gpu-tools
    libGL
    libnotify
    lshw
    neofetch
    openssh
    openssl
    patchelf
    pkg-config
    qrencode
    ripgrep
    tokei
    tree
    xcape
    xclip
    xdo
    xdotool
    xfontsel
    xorg.setxkbmap
    xorg.xev
    xz
    zip unzip
  ];
}
