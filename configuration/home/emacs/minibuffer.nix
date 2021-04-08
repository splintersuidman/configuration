{ pkgs, ... }:
let
  fd = "${pkgs.fd}/bin/fd";
  ripgrep = "${pkgs.ripgrep}/bin/rg";
in {
  programs.emacs.init.usePackage = {
    selectrum = {
      enable = true;
      after = [ "general" ];
      config = ''
        (selectrum-mode)
        (general-define-key
         :keymaps 'selectrum-minibuffer-map
         "C-u" 'delete-minibuffer-contents)
      '';
    };

    prescient = {
      enable = true;
      config = ''
        (prescient-persist-mode)
      '';
    };

    selectrum-prescient = {
      enable = true;
      after = [ "selectrum" "prescient" ];
      init = ''
        (setq selectrum-prescient-enable-filtering nil)
      '';
      config = ''
        (selectrum-prescient-mode)
      '';
    };

    orderless = {
      enable = true;
      after = [ "selectrum" ];
      init = ''
        (setq completion-styles '(orderless))
        (setq orderless-skip-highlighting (lambda () selectrum-is-active))
        (setq selectrum-highlight-candidates-function 'orderless-highlight-matches)
      '';
    };

    marginalia = {
      enable = true;
      init = ''
        (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
        (marginalia-mode)
        (advice-add 'marginalia-cycle :after
                    (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit 'keep-selected))))
      '';
    };

    consult = {
      enable = true;
      after = [ "general" ];
      init = ''
        (setq consult-find-command "${fd} ARG . OPTS")
        (setq consult-ripgrep-command "${ripgrep} --null --line-buffered --color=always --max-columns=500   --no-heading --line-number . -e ARG OPTS")
      '';
      config = ''
        (general-define-key
         :prefix my-leader
         :states '(normal visual motion)
         :keymaps 'override
         "/" '(consult-line :which-key "Search line"))
      '';
    };

    embark = {
      enable = true;
      after = [ "general" ];
      init = ''
        (defun refresh-selectrum ()
          (setq selectrum--previous-input-string nil))
        (add-hook 'embark-pre-action-hook 'refresh-selectrum)
      '';
      config = ''
        (general-define-key
         :keymaps 'selectrum-minibuffer-map
         "C-o" 'embark-act)
      '';
    };
  };
}
