{ pkgs, ... }: {
  home.packages =
    [ pkgs.unstable.signal-desktop pkgs.unstable.discord ];
}
