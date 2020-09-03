{ pkgs, ... }:
let
  zathura = "${pkgs.zathura}/bin/zathura";
in
{
  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: {
      inherit (tpkgs) scheme-full;
    };
  };

  programs.emacs.init.usePackage = {
    tex-mode = {
      enable = false;
      after = [ "evil-leader" ];
      extraConfig = ''
        :defines TeX-engine
      '';
      init = ''
        (setq TeX-engine 'xetex)
      '';
      config = ''
        (evil-leader/set-key-for-mode 'tex-mode
          "cc" 'TeX-command-master
          "cv" 'TeX-view)
      '';
    };

    latex = {
      enable = true;
      mode = [ ''("\\.tex\\'" . latex-mode)'' ];
      package = epkgs: epkgs.auctex;
      init = ''
        (setq-default TeX-PDF-mode t)
        (setq-default TeX-engine 'xetex)
        (setq TeX-auto-save t)
        (setq TeX-parse-self t)
        (setq-default TeX-master nil)
        (setq TeX-output-view-style
              '(("^pdf$" "." "${zathura} %o")
                ( "^ps$" "." "${zathura} %o")
                ("^dvi$" "." "${zathura} %o")))
      '';
      config = ''
        (setf (alist-get 'output-pdf TeX-view-program-selection)
              '("Zathura"))
        '';
    };
  };
}
