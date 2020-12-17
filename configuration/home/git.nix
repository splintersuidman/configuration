{ pkgs, config, ... }: {
  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "Splinter Suidman";
    userEmail = "splinter@mannenopdemaan.nl";

    extraConfig = { pull.rebase = false; };

    lfs.enable = true;
  };

  programs.emacs.init.usePackage = {
    magit = { enable = true; };

    magit-todos = {
      enable = true;
      hook = [ "(magit-mode . magit-todos-mode)" ];
    };

    evil-magit = {
      enable = true;
      after = [ "evil" "magit" ];
    };

    git-gutter = {
      enable = true;
      config = ''
        (global-git-gutter-mode)
      '';
    };

    browse-at-remote = {
      enable = true;
      config = ''
      '';
    };
  };
}
