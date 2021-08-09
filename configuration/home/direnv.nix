{ pkgs, ... }: {
  programs.bash.initExtra = ''
    eval "$(${pkgs.direnv}/bin/direnv hook bash)"
  '';
  home.packages = [ pkgs.direnv ];

  programs.emacs.init.modules."init/init-direnv.el" = {
    enable = true;
    config = ./direnv.el;
    feature = "init-direnv";
  };
}
