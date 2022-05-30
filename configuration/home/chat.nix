{ pkgs, ... }: {
  home.packages =
    [ pkgs.unstable.signal-desktop pkgs.qtox pkgs.unstable.discord pkgs.tdesktop ];
}
