{ config, pkgs, ... }:
{
  services.lorri.enable = true;
  programs.bash.initExtra = ''
    eval "$(${pkgs.direnv}/bin/direnv hook bash)"
  '';
  home.packages = [
    pkgs.direnv
  ];
}
