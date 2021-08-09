{ pkgs, ... }: {
  home.packages = [ pkgs.guile ];

  programs.emacs.init.modules."init/init-scheme.el" = {
    enable = true;
    config = ./scheme.el;
    feature = "init-scheme";
  };
}
