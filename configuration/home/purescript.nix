{ pkgs, config, ... }: {
  home.packages = with pkgs; [
    purs
    spago-unstable
    purs-tidy-bin.purs-tidy-0_10_0
    purs-backend-es
    purescript-language-server
  ];

  programs.emacs.init.modules."init/init-purescript.el" = {
    enable = true;
    config = ./purescript.el;
    feature = "init-purescript";
  };
}
