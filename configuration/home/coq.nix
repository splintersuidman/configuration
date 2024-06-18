{ pkgs, config, ... }: {
  home.packages = [ pkgs.coq ];

  programs.emacs.init.modules."init/init-coq.el" = {
    enable = true;
    config = ./coq.el;
    feature = "init-coq";
  };
}
