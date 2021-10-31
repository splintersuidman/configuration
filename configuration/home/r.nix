{ pkgs, ... }: {
  home.packages = [ pkgs.R ];

  programs.emacs.init.modules."init/init-r.el" = {
    enable = true;
    config = ./r.el;
    feature = "init-r";
  };
}
