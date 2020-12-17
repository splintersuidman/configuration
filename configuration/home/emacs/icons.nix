{ pkgs, ... }: {
  home.packages = [ pkgs.emacs-all-the-icons-fonts ];

  programs.emacs.init.usePackage = {
    all-the-icons = { enable = true; };

    all-the-icons-dired = {
      enable = true;
      hook = [ "(dired-mode . all-the-icons-dired-mode)" ];
    };
  };
}
