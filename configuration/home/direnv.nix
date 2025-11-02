{ pkgs, ... }: {
  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    enableNushellIntegration = true;
  };

  programs.emacs.init.modules."init/init-direnv.el" = {
    enable = true;
    config = ./direnv.el;
    feature = "init-direnv";
  };
}
