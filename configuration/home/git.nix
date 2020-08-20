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
  };

  programs.emacs.init.usePackage = {
    magit = {
      enable = true;
      after = [ "evil-leader" ];
      init = ''
        (evil-leader/set-key
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
      after = [ "evil-leader" ];
      config = ''
        (global-git-gutter-mode)
        (evil-leader/set-key
          "gn" 'git-gutter:next-diff
          "gp" 'git-gutter:previous-diff)
      '';
    };

    browse-at-remote = {
      enable = true;
      after = [ "evil-leader" ];
      config = ''
        (evil-leader/set-key
          "gr" 'browse-at-remote)
      '';
    };
  };
}
