{ ... }: {
  programs.emacs.init.modules."init/init-elfeed.el" = {
    enable = true;
    config = ./elfeed.el;
    feature = "init-elfeed";
  };
}
