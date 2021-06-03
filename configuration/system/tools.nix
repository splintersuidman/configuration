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
    libnotify
    lshw
    openssh
    openssl
    patchelf
    tmux
    tree
    xclip
    xdotool
    xorg.xev
    xz
    zip unzip
  ];
}
