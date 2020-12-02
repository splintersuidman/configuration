{ pkgs, ... }:
{
  programs.emacs.init.usePackage = {
    dumb-jump = {
      enable = true;
      after = [ "general" ];
      config = ''
        ;; TODO: doesn't seem to work.
        (general-define-key
          :prefix my-leader
          :states '(normal visual motion)
          :keymaps 'override
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
