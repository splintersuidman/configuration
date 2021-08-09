{ pkgs, config, ... }: {
  home.packages = [ pkgs.gcc pkgs.binutils pkgs.indent pkgs.ccls ];

  programs.emacs.init.modules."init/init-c.el" = {
    enable = true;
    config = ./c.el;
    feature = "init-c";
  };
}
