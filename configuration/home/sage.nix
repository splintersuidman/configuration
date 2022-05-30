{ pkgs, ... }: {
  home.packages = [ pkgs.sage ];

  programs.emacs.init.modules."init/init-sage.el" = {
    enable = true;
    config = ./sage.el;
    feature = "init-sage";
  };
}
