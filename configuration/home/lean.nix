{ pkgs, ... }: {
  home.packages = [ pkgs.elan ];

  programs.emacs.init.modules."init/init-lean.el" = {
    enable = false;
    config = ./lean.el;
    feature = "init-lean";
  };
}
