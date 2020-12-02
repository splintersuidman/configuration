{ pkgs, config, ... }:
{
  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "Splinter Suidman";
    userEmail = "splinter@mannenopdemaan.nl";

    extraConfig = {
      pull.rebase = false;
    };

    lfs.enable = true;
  };

  programs.emacs.init.usePackage = {
    magit = {
      enable = true;
      after = [ "general" ];
      init = ''
        (general-define-key
          :prefix my-leader
          :states '(normal visual motion)
          :keymaps 'override
          "gg" 'magit-status)
      '';
    };

    magit-todos = {
      enable = true;
      hook = [
        "(magit-mode . magit-todos-mode)"
      ];
    };

    evil-magit = {
      enable = true;
      after = [ "evil" "magit" ];
    };

    git-gutter = {
      enable = true;
      after = [ "general" ];
      config = ''
        (global-git-gutter-mode)
        (general-define-key
          :prefix my-leader
          :states '(normal visual motion)
          :keymaps 'override
          "gn" 'git-gutter:next-diff
          "gp" 'git-gutter:previous-diff)
      '';
    };

    browse-at-remote = {
      enable = true;
      after = [ "general" ];
      config = ''
        (general-define-key
          :prefix my-leader
          :states '(normal visual motion)
          :keymaps 'override
          "gr" 'browse-at-remote)
      '';
    };
  };
}
