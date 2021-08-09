{ pkgs, config, ... }: {
  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "Splinter Suidman";
    userEmail = "splinter@mannenopdemaan.nl";

    extraConfig = {
      pull.rebase = false;
      # For forge
      github.username = "splintah";
    };

    lfs.enable = true;

  };

  programs.emacs.init.modules."init/init-git.el" = {
    enable = true;
    config = ./git.el;
    feature = "init-git";
  };
}
