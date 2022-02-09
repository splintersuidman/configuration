{ pkgs, config, ... }:
let
  agda = pkgs.unstable.agda.withPackages (agdaPkgs:
    with agdaPkgs; [
      standard-library
      cubical
      agda-categories
      agdarsec
    ]);
in {
  home.packages = [ agda ];

  home.file.".agda/defaults".text = ''
    standard-library
    agda-categories
  '';

  programs.emacs.init.modules."init/init-agda.el" = {
    enable = true;
    config = ./agda.el;
    feature = "init-agda";
  };
}
