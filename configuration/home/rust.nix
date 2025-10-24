{ pkgs, config, ... }: {
  home.packages = [
    (pkgs.fenix.complete.withComponents [
      "cargo"
      "clippy"
      "rust-src"
      "rustc"
      "rustfmt"
    ])
    pkgs.rust-analyzer-nightly
  ];

  programs.emacs.init.modules."init/init-rust.el" = {
    enable = true;
    config = ./rust.el;
    feature = "init-rust";
  };
}
