{ config, pkgs, ... }:
{
  programs.urxvt = {
    enable = true;
    extraConfig = {};
    keybindings = {
      # TODO: selecting and copying does not work from tmux.
      "Shift-Control-C" = "eval:selection_to_clipboard";
      "Shift-Control-V" = "eval:paste_clipboard";
    };
    fonts = [ "xft:DejaVu Sans Mono:pixelsize=12:antialias=true:autohint=true" ];
    iso14755 = false;
    scroll = {
      bar.enable = false;
      scrollOnKeystroke = true;
      scrollOnOutput = false;
    };
  };
}
