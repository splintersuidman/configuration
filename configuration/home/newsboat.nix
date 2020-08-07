{ pkgs, config, ... }:
let
  browser = config.programs.browser.program;
  mpv = "${pkgs.mpv}/bin/mpv";
in
{
  programs.newsboat = {
    enable = true;
    autoReload = true;
    browser = "${browser} %u";
    extraConfig = ''
      # Vim-style keys
      bind-key j down
      bind-key k up
      bind-key j next articlelist
      bind-key k prev articlelist
      bind-key J next-feed articlelist
      bind-key K prev-feed articlelist
      bind-key G end
      bind-key g home
      bind-key d pagedown
      bind-key u pageup
      bind-key l open
      bind-key h quit
      bind-key a toggle-article-read
      bind-key A mark-feed-read
      bind-key n next-unread
      bind-key N prev-unread
      bind-key U show-urls
      bind-key D pb-download
      bind-key x pb-delete
      bind-key A pb-toggle-download-all

      color listnormal default default
      color listfocus white default bold
      color listnormal_unread blue default
      color listfocus_unread blue default bold
      color info white black bold
      color article default default

      # Macros
      macro v set browser "${mpv} %u" ; open-in-browser ; set browser "${browser}"

      # Podcast configuration
      download-path "${config.xdg.userDirs.music}/podcasts/%n/"
      download-filename-format "%t.%e"
      max-downloads 4
      player "${mpv} --no-video"

      # Notification configuration
      # notify-always no
      # notify-program "${pkgs.libnotify}/bin/notify-send"
      # notify-format "Newsboat: %d new articles"
    '';

    # NOTE: This file is not contained in the git index.
    urls = import ./newsboat/urls.nix;
  };
}
