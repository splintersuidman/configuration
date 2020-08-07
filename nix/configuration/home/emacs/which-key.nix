{ ... }:
{
  programs.emacs.init.usePackage = {
    which-key = {
      enable = true;
      config = ''
        (which-key-mode)
      '';
    };
  };
}
