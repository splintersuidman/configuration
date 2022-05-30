{ pkgs, ... }: {
  home.packages = with pkgs; [
    zoom-us

    # Files
    ranger

    # Images
    sxiv
    scrot
    gimp
    inkscape
    imagemagick

    # Type setting
    texmacs

    # Programming tools
    gnumake
    gdb

    # Man pages
    man-pages
    # stdmanpages # C++ std documentation manpages

    # Type setting
    pandoc
    groff

    # Video
    unstable.youtube-dl
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
    patchelf
    pkg-config
    qrencode
    ripgrep
    tokei
    tree
    unzip
    xclip
    xdo
    xdotool
    xorg.setxkbmap
    xorg.xev
    xz
    zip

    # Networking
    curl
    dsniff
    nmap
    openssh
    openssl
    traceroute
    transmission-gtk
    wget
    whois
    wireshark
  ];
}
