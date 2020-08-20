{ pkgs, ... }:
{
  programs.emacs.init.usePackage = {
    dumb-jump = {
      enable = true;
      after = [ "evil-leader" ];
      config = ''
        (evil-leader/set-key
          "jj" 'dumb-jump-go
          "jo" 'dumb-jump-go-other-window
          "je" 'dumb-jump-go-prefer-external
          "jx" 'dumb-jump-go-prefer-external-other-window
          "ji" 'dumb-jump-prompt
          "jl" 'dumb-jump-quick-look
          "jb" 'dumb-jump-back)
      '';
    };
  };
}
