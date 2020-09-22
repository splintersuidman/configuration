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
      '';
    };
  };
}
