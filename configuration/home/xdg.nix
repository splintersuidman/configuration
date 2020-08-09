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
      desktop = "${home}/Desktop";
      documents = "${home}/Documents";
      download = "${home}/Downloads";
      pictures = "${home}/Pictures";
      music = "${home}/Music";
      videos = "${home}/Videos";
    };
    mimeApps = {
      enable = false;
      # TODO:
      # associations = {};
    };
  };

  # Also set session variables for XDG User Directories.
  home.sessionVariables = with config.xdg.userDirs; {
    XDG_DESKTOP_DIR = desktop;
    XDG_DOCUMENTS_DIR = documents;
    XDG_DOWNLOAD_DIR = download;
    XDG_MUSIC_DIR = music;
    XDG_PICTURES_DIR = pictures;
    XDG_PUBLICSHARE_DIR = publicShare;
    XDG_TEMPLATES_DIR = templates;
    XDG_VIDEOS_DIR = videos;
  };
}
