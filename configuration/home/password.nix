{ config, pkgs, ... }: {
  home.packages = [
    # pkgs.krunner-pass
    # pkgs.wl-clipboard
    pkgs.bitwarden-cli
    pkgs.bitwarden-desktop
  ];
  programs.password-store = {
    enable = false;
    settings = {
      PASSWORD_STORE_DIR = "${config.xdg.dataHome}/password-store";
    };
  };
  programs.browserpass.enable = false;
}
