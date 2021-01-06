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

    git-gutter = let
      git-gutter-fringe-enabled =
        config.programs.emacs.init.usePackage.git-gutter-fringe.enable;
    in {
      enable = true;
      init = if git-gutter-fringe-enabled then ''
        (setq git-gutter:init-function 'git-gutter-fr:init
              git-gitter:view-diff-function 'git-gutter-fr:view-diff-infos
              git-gutter:clear-function 'git-gutter-fr:clear
              git-gutter:window-width -1)
      '' else
        "";
      config = ''
        (global-git-gutter-mode)
      '';
    };

    git-gutter-fringe = {
      enable = true;
      after = [ "git-gutter" ];
      init = ''
        (setq fringes-outside-margins t)
      '';
      config = ''
        (define-fringe-bitmap 'git-gutter-fr:added [224]
          nil nil '(center repeated))
        (define-fringe-bitmap 'git-gutter-fr:modified [224]
          nil nil '(center repeated))
        (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
          nil nil 'bottom)
      '';
    };

    browse-at-remote = { enable = true; };
  };
}
