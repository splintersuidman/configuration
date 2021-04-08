{ pkgs, ... }: {
  home.packages = [ pkgs.guile ];

  programs.emacs.init.usePackage = {
    scheme = {
      enable = true;
      init = ''
        (setq scheme-program-name "${pkgs.guile}/bin/guile")
      '';
    };

    geiser = {
      enable = false;
      after = [ "scheme" "company" "general" ];
      init = ''
        (setq geiser-active-implementations '(guile))
      '';
      config = ''
        (general-define-key
          :prefix my-local-leader
          :states '(normal visual motion)
          :keymaps 'scheme-mode-map
          "\\" 'geiser-insert-lambda
          "b" 'geiser-eval-buffer
          "c" 'geiser-eval-definition
          "e" 'geiser-eval-last-sexp
          "k" 'geiser-compile-current-buffer
          "l" 'geiser-load-file
          "L" 'geiser-load-current-buffer
          "r" 'geiser-eval-region
          "s" 'geiser-set-scheme
          "z" 'geiser-doc-switch-to-repl)
      '';
    };
  };
}
