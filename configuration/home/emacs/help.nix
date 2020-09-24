{ ... }: {
  programs.emacs.init.usePackage = {
    helpful = {
      enable = true;
      after = [ "evil-leader" "counsel" ];
      init = ''
        (setq counsel-describe-function-function 'helpful-callable)
        (setq counsel-describe-variable-function 'helpful-variable)
      '';
    };
  };
}
