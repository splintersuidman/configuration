{ pkgs, config, ... }: {
  programs.git = {
    package = pkgs.gitFull;
    enable = true;
    settings = {
      user = {
        name = "Splinter Suidman";
        email = "splinter@mannenopdemaan.nl";
        pull.rebase = false;
        # For forge
        github.user = "splintersuidman";
      };
    };

    lfs.enable = true;
  };

  programs.emacs.init.modules."init/init-git.el" = {
    enable = true;
    config = ./git.el;
    feature = "init-git";
  };
}
