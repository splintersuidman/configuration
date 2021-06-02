{ ... }:
{
  programs.emacs.init = {
    prelude = ''
      ;; Use spaces instead of tabs.
      (setq-default indent-tabs-mode nil)
    '';

    usePackage.aggressive-indent = {
      enable = false;
    };
  };
}
