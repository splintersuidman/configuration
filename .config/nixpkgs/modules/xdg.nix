{ pkgs, config, ... }:
let home = config.home.homeDirectory; in
{
  xdg = {
    enable = true;
    cacheHome = "${home}/.cache";
    configHome = "${home}/.config";
    dataHome = "${home}/.local/share";
    userDirs = {
      enable = true;
      desktop = "${home}/desktop";
      documents = "${home}/documenten";
      download = "${home}/downloads";
      music = "${home}/muziek";
      videos = "${home}/video";
    };
    mimeApps = {
      enable = false;
      # TODO:
      # associations = {};
    };
  };
}
