{ config, ... }: {
  programs.opam = {
    enable = true;
    enableBashIntegration = true;
  };

  home.sessionVariables = { OPAMROOT = "${config.xdg.dataHome}/opam"; };

  programs.emacs.init.modules."init/init-ocaml.el" = {
    enable = true;
    config = ./ocaml.el;
    feature = "init-ocaml";
  };
}
