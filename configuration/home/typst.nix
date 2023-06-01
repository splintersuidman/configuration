{ pkgs, ... }: {
  home.packages = [
    pkgs.unstable.typst
    pkgs.unstable.typst-lsp
  ];

  programs.emacs.init.modules."init/init-typst.el" = {
    enable = true;
    config = ./typst.el;
    feature = "init-typst";
  };
}
