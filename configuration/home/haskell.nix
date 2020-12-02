{ pkgs, config, ... }:
let
  hls =
    "${pkgs.haskellPackages.haskell-language-server}/bin/haskell-language-server-wrapper";
  eglotEnable = config.programs.emacs.init.usePackage.eglot.enable;
in {
  home.packages = with pkgs; [
    ghc
    cabal-install
    stack
    # haskellPackages.pointfree
    haskellPackages.hoogle
    haskellPackages.hlint
    haskellPackages.ghcid
    haskellPackages.stylish-haskell
    haskellPackages.hasktags
    cabal2nix
    haskellPackages.ghcide
  ];

  # :def pf \s -> pure $ ":!${pointfree}/bin/pointfree \"" <> s <> "\""
  home.file.".ghc/ghci.conf".text = with pkgs.haskellPackages; ''
    :def hoogle \s -> pure $ ":!${hoogle}/bin/hoogle --count=15 \"" <> s <> "\""
    :def hlint \s -> pure $ ":!${hlint}/bin/hlint \"" <> s <> "\""
    :set prompt "λ> "
    :set +t
  '';

  # TODO: investigate using ghcide / language server.
  programs.emacs.init.usePackage = {
    haskell-mode = {
      enable = true;
      after = [ "general" ] ++ (if eglotEnable then [ "eglot" ] else [ ]);
      mode = [
        ''("\\.hs\\'" . haskell-mode)''
        ''("\\.hsc\\'" . haskell-mode)''
        ''("\\.c2hs\\'" . haskell-mode)''
        ''("\\.cpphs\\'" . haskell-mode)''
        ''("\\.lhs\\'" . haskell-literate-mode)''
      ];
      hook = [
        "(haskell-mode . interactive-haskell-mode)"
        "(haskell-mode . haskell-auto-insert-module-template)"
        ''
          (haskell-mode .
            (lambda ()
              (set (make-local-variable 'company-backends)
                   (append '((company-capf company-dabbrev-code))
                           company-backends))))
        ''
      ];
      init = ''
        (setq haskell-stylish-on-save t)
        (setq haskell-tags-on-save nil)
        (setq haskell-process-suggest-remove-import-lines t)
        (setq haskell-process-log t)
        (setq haskell-process-type 'auto)

        (defun haskell-process-load-file-choose-type ()
          "Ask the user to choose a value for `haskell-process-type',
        and load the current file using that process type."
          (interactive)
          (require 'ido)
          (let* ((process-types (mapcar 'cadr
                                        (cdr (custom-variable-type 'haskell-process-type))))
                 (choices (mapcar 'symbol-name process-types))
                 (choice (ido-completing-read "Haskell process type: " choices))
                 (process-type (intern choice)))
            (let ((haskell-process-type process-type))
              (haskell-process-load-file))))
      '' + (if eglotEnable then ''
        (add-to-list 'eglot-server-programs '(haskell-mode . ("${hls}" "--lsp")))
      '' else
        "");

      config = ''
        (general-define-key
          :prefix my-local-leader
          :states '(normal visual motion)
          :keymaps 'interactive-haskell-mode-map
          "," 'haskell-mode-format-imports
          "b" 'haskell-interactive-switch
          "c" 'haskell-process-cabal-build
          "ef" 'haskell-goto-first-error
          "en" 'haskell-goto-next-error
          "ep" 'haskell-goto-prev-error
          "f" 'haskell-mode-stylish-buffer
          "l" 'haskell-process-load-file
          "L" 'haskell-process-load-file-choose-type
          "k" 'haskell-interactive-mode-clear
          "r" 'haskell-process-reload
          "s" 'haskell-mode-stylish-buffer
          "t" 'haskell-process-do-type
          "v" 'haskell-cabal-visit-file
          "x" 'haskell-process-cabal)

        (general-define-key
          :prefix my-local-leader
          :states '(normal visual motion)
          :keymaps 'haskell-interactive-mode-map
          "c" 'haskell-process-interrupt
          "f" 'next-error-follow-minor-mode
          "k" 'haskell-interactive-mode-clear
          "n" 'haskell-interactive-mode-prompt-next
          "p" 'haskell-interactive-mode-prompt-previous
          "z" 'haskell-interactive-switch-back)
      '';
    };

    lsp-haskell = {
      enable = config.programs.emacs.init.usePackage.lsp-mode.enable;
      init = ''
        (setq lsp-haskell-process-path-hie "ghcide")
        (setq lsp-haskell-process-args-hie '())
      '';
    };
  };
}
