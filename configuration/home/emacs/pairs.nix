{ ... }:
{
  programs.emacs.init.usePackage = {
    paren = {
      enable = true;
      init = ''
        (setq show-paren-delay 0)
      '';
      config = ''
        (show-paren-mode)
      '';
    };

    elec-pair = {
      enable = true;
      config = ''
        (electric-pair-mode)
      '';
    };
  };
}
