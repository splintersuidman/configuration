{ ... }: {
  programs.emacs.init.usePackage = {
    pdf-tools = {
      enable = true;
      after = [ "general" ];
      mode = [ ''("\\.pdf\\'" . pdf-view-mode)'' ];
      # NOTE: see https://github.com/politza/pdf-tools/issues/528#issuecomment-564773133.
      config = ''
        (pdf-tools-install :no-query)
        (require 'pdf-occur)
        ;; Unbind leader key, as pdf-tools binds SPC.
        (general-define-key
          :prefix my-leader
          :states '(normal visual motion)
          :keymaps 'pdf-view-mode-map
          "" nil)
        (general-define-key
          :prefix my-local-leader
          :states '(normal visual motion)
          :keymaps 'pdf-view-mode-map
          "r" 'pdf-view-midnight-minor-mode)

        (defun splinter-set-pdf-view-midnight-colors (enable)
          (setq pdf-view-midnight-colors
                (cons
                 (face-attribute 'default :foreground)
                 (face-attribute 'default :background))))
        (advice-add 'pdf-view-midnight-minor-mode
                    :before 'splinter-set-pdf-view-midnight-colors)
      '';
    };
  };
}
