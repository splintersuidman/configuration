{ pkgs, config, ... }:
{
  xdg = {
    mimeApps = {
      enable = false;
      # TODO:
      # associations = {};
    };
    userDirs = {
      enable = true;
      desktop = "${config.home.homeDirectory}/desktop";
      documents = "${config.home.homeDirectory}/documents";
      download = "${config.home.homeDirectory}/downloads";
      music = "${config.home.homeDirectory}/muziek";
      videos = "${config.home.homeDirectory}/video";
    };
  };
}
