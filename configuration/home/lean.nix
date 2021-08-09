{ pkgs, ... }: {
  home.packages = [ pkgs.elan pkgs.mathlibtools ];

  programs.emacs.init.modules."init/init-lean.el" = {
    enable = true;
    config = ./lean.el;
    feature = "init-lean";
  };
}
