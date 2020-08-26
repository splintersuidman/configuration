{ pkgs, ... }:
{
  home.packages = [
    pkgs.guile
  ];

  programs.emacs.init.usePackage = {
    scheme = {
      enable = true;
      init = ''
        (setq scheme-program-name "${pkgs.guile}/bin/guile")
      '';
    };

    geiser = {
      enable = true;
      after = [ "scheme" "company" "evil-leader" ];
      init = ''
        (setq geiser-active-implementations '(guile))
      '';
      config = ''
        (evil-leader/set-key-for-mode 'scheme-mode
          "c\\" 'geiser-insert-lambda
          "cb" 'geiser-eval-buffer
          "cc" 'geiser-eval-definition
          "ce" 'geiser-eval-last-sexp
          "ck" 'geiser-compile-current-buffer
          "cl" 'geiser-load-file
          "cL" 'geiser-load-current-buffer
          "cr" 'geiser-eval-region
          "cs" 'geiser-set-scheme
          "cz" 'geiser-doc-switch-to-repl)
      '';
    };
  };
}
