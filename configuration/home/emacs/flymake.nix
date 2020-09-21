{ ... }: {
  programs.emacs.init.usePackage = {
    flymake = { enable = true; };
    flymake-diagnostic-at-point = {
      enable = true;
      after = [ "evil-leader" ];
      hook = [
        "(flymake-mode . flymake-diagnostic-at-point-mode)"
      ];
      config = ''
        (evil-leader/set-key
          "ln" 'flymake-goto-next-error
          "lp" 'flymake-goto-prev-error)
      '';
    };
  };
}
