{ pkgs, config, ... }:
{
  programs.mpv = {
    enable = true;
    scripts = with pkgs.mpvScripts; [ mpris ];
  };
}
