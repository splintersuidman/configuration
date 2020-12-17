{ ... }: {
  programs.emacs.init.usePackage = {
    flymake = { enable = true; };
    flymake-diagnostic-at-point = {
      enable = true;
      hook = [
        "(flymake-mode . flymake-diagnostic-at-point-mode)"
      ];
    };
  };
}
