{ pkgs, ... }:
let
  ripgrep = "${pkgs.ripgrep}/bin/rg";
  fzf = "${pkgs.fzf}/bin/fzf";
in {
  programs.emacs.init.usePackage = {
    ivy = {
      enable = true;
      init = ''
        (setq ivy-use-virtual-buffers t)
        (setq enable-recursive-minibuffers t)
      '';
      config = ''
        (ivy-mode 1)
      '';
    };

    ivy-rich = {
      enable = true;
      config = ''
        (ivy-rich-mode 1)
      '';
    };

    counsel = {
      enable = true;
      init = ''
        (setq counsel-rg-base-command "${ripgrep} --with-filename --no-heading --line-number --color never %s")
        (setq counsel-fzf-cmd "${fzf} -f \"%s\"")
      '';
      config = ''
        (defun my/counsel-rg ()
          "Calls `COUNSEL-RG' with \"rg: \" as the prompt; otherwise, the
        path in the Nix store will be included in the prompt."
          (interactive)
          (counsel-rg nil nil nil "rg: "))
      '';
      bind = {
        "M-x" = "counsel-M-x";
        "C-x C-f" = "counsel-find-file";
        "C-x b" = "counsel-switch-buffer";
        "C-x d" = "counsel-dired";
      };
    };

    swiper = {
      enable = true;
      after = [ "evil-leader" ];
      config = ''
        (evil-leader/set-key "ss" 'swiper)
      '';
    };
  };
}
