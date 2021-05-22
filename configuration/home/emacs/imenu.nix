{ ... }: {
  programs.emacs.init.usePackage = {
    imenu = {
      enable = true;
      init = ''
        (setq imenu-max-item-length nil)
      '';
    };
  };
}
