{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    manpages

    # Editors
    emacs
    vim
    ed

    # Files
    wget
    curl

    # Other/system tools
    brightnessctl
    coreutils
    file
    gnupg
    gnutar
    intel-gpu-tools
    libGL
    libnotify
    lshw
    openssh
    openssl
    patchelf
    tmux
    tree
    xcape
    xclip
    xdotool
    xfontsel
    xorg.xev
    xz
    zip unzip
  ];
}
