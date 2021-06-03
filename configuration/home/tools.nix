{ pkgs, ... }: {
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
    gdb

    # Man pages
    manpages
    # stdmanpages # C++ std documentation manpages

    # Type setting
    pandoc
    groff

    # Video
    unstable.youtubeDL
    ffmpeg

    # Other/system tools
    aspell
    aspellDicts.en
    aspellDicts.nl
    calc
    coreutils
    entr
    fd
    file
    gnupg
    gnutar
    hplip
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
    traceroute
    tree
    whois
    xclip
    xdo
    xdotool
    xorg.setxkbmap
    xorg.xev
    xz
    zip
    unzip
  ];
}
