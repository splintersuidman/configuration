{ pkgs, ... }: {
  home.packages = [ pkgs.just pkgs.just-lsp ];

  programs.emacs.init.modules."init/init-just.el" = {
    enable = true;
    config = ./just.el;
    feature = "init-just";
  };
}
