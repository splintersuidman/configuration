{ ... }: {
  programs.emacs.init.usePackage = {
    flymake = { enable = true; };
    flymake-diagnostic-at-point = {
      enable = true;
      after = [ "general" ];
      hook = [
        "(flymake-mode . flymake-diagnostic-at-point-mode)"
      ];
      config = ''
        (general-define-key
          :prefix my-leader
          :states '(normal visual motion)
          "ln" 'flymake-goto-next-error
          "lp" 'flymake-goto-prev-error)
      '';
    };
  };
}
