{ ... }: {
  programs.emacs.init.modules."init/init-markdown.el" = {
    enable = true;
    config = ./markdown.el;
    feature = "init-markdown";
  };
}
