{ config, pkgs, ... }: {
  home.packages = [ pkgs.krunner-pass pkgs.wl-clipboard ];
  programs.password-store = {
    enable = true;
    settings = {
      PASSWORD_STORE_DIR = "${config.xdg.dataHome}/password-store";
    };
  };
  programs.browserpass.enable = true;
}
