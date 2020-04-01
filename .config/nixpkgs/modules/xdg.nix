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
      documents = "${home}/docs";
      download = "${home}/downloads";
      pictures = "${home}/images";
      music = "${home}/audio";
      videos = "${home}/video";
    };
    mimeApps = {
      enable = false;
      # TODO:
      # associations = {};
    };
  };
}
