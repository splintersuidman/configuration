{ pkgs, ... }:
{
  programs.bash.initExtra = ''
    eval "$(${pkgs.direnv}/bin/direnv hook bash)"
  '';
  home.packages = [
    pkgs.direnv
  ];

  programs.emacs.init.usePackage = {
    direnv = {
      enable = true;
      init = ''
        (setq direnv-always-show-summary nil)
      '';
      config = ''
        (direnv-mode)
      '';
    };
  };
}
