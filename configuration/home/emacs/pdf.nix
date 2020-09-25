{ ... }: {
  programs.emacs.init.usePackage = {
    pdf-tools = {
      enable = true;
      after = [ "evil-leader" ];
      mode = [
        ''("\\.pdf\\'" . pdf-view-mode)''
      ];
      # NOTE: see https://github.com/politza/pdf-tools/issues/528#issuecomment-564773133.
      config = ''
        (pdf-tools-install :no-query)
        (require 'pdf-occur)
        (evil-leader/set-key-for-mode 'pdf-view-mode
          "cr" 'pdf-view-midnight-minor-mode)

        (defun my/set-pdf-view-midnight-colors (enable)
          (setq pdf-view-midnight-colors
                (cons
                 (face-attribute 'default :foreground)
                 (face-attribute 'default :background))))
        (advice-add 'pdf-view-midnight-minor-mode
                    :before 'my/set-pdf-view-midnight-colors)
      '';
    };
  };
}
