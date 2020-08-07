{ ... }:
{
  programs.emacs.init.usePackage = {
    ido = {
      enable = true;
      init = ''
        (setq ido-enable-flex-matching t)
        (setq ido-create-new-buffer 'always)
        (setq ido-everywhere t)
      '';
      config = ''
        (ido-mode)
      '';
    };

    ido-vertical-mode = {
      enable = true;
      after = [ "ido" ];
      init = ''
        (setq ido-vertical-define-keys 'C-n-and-C-p-only)
      '';
      config = ''
        (ido-vertical-mode)
      '';
    };
  };
}
