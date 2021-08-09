{ ... }: {
  programs.emacs.init.modules."init/init-yaml.el" = {
    enable = true;
    config = ./yaml.el;
    feature = "init-yaml";
  };
}
