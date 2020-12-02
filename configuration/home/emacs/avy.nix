{ ... }:
{
  programs.emacs.init.usePackage = {
    avy = {
      enable = true;
      after = [ "general" ];
      config = ''
        ;; TODO: doesn't seem to work.
        (general-define-key
          :prefix my-leader
          :states '(normal visual motion)
          :keymaps 'override
          "s" '(avy-goto-char :which-key "Goto char"))
      '';
    };
  };
}
