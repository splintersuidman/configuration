{ ... }:
{
  programs.emacs.init.usePackage = {
    avy = {
      enable = true;
      after = [ "evil-leader" ];
      config = ''
        (evil-leader/set-key "sf" 'avy-goto-char)
      '';
    };
  };
}
