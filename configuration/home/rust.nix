{ pkgs, config, ... }: {
  home.packages = [ pkgs.rustup ];

  programs.emacs.init.modules."init/init-rust.el" = {
    enable = true;
    config = ./rust.el;
    feature = "init-rust";
  };
}
