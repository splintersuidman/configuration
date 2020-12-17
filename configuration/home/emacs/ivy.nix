{ pkgs, ... }:
let ripgrep = "${pkgs.ripgrep}/bin/rg";
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

    all-the-icons-ivy = {
      enable = true;
      init = ''
        (setq all-the-icons-spacer " ")
      '';
      hook = [ "(after-init . all-the-icons-ivy-setup)" ];
    };

    counsel = {
      enable = true;
      init = ''
        (setq counsel-rg-base-command "${ripgrep} --with-filename --no-heading --line-number --color never %s")
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
      after = [ "general" ];
      config = ''
        (general-define-key
          :prefix my-leader
          :states '(normal visual motion)
          :keymaps 'override
          "/" '(swiper :which-key "Swiper"))
      '';
    };

    prescient = {
      enable = true;
      init = ''
        (setq prescient-filter-method '(fuzzy))
      '';
      config = ''
        (prescient-persist-mode)
      '';
    };

    ivy-prescient = {
      enable = true;
      after = [ "ivy" "counsel" "prescient" ];
      config = ''
        (ivy-prescient-mode)
      '';
    };
  };
}
