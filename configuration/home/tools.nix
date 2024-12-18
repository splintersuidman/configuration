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
    unstable.yt-dlp
    ffmpeg
    vlc

    # Audio
    audacity
    plasma5Packages.kdenlive

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
    unstable.transmission_4-qt
    wget
    whois
    wireshark
  ];
}
