{ pkgs, ... }:
let zathura = "${pkgs.zathura}/bin/zathura";
in {
  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: { inherit (tpkgs) scheme-full; };
  };

  programs.emacs.init.usePackage = {
    latex = {
      enable = true;
      after = [ "general" ];
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
        ;; Revert TeX document buffer after compilation.
        (add-hook 'TeX-after-compilation-finished-functions
                  'TeX-revert-document-buffer)
        (setf (alist-get 'output-pdf TeX-view-program-selection)
              '("PDF Tools"))

        (general-define-key
          :prefix my-local-leader
          :states '(normal visual motion)
          :keymaps 'LaTeX-mode-map
          "c" 'TeX-command-master
          "e" 'LaTeX-environment
          "s" 'LaTeX-section
          "v" 'TeX-view)
      '';
    };
  };
}
