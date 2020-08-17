{ pkgs, ... }:
{
  home.packages = with pkgs; [
    ghc
    cabal-install
    stack
    haskellPackages.hindent
    haskellPackages.pointfree
    haskellPackages.hoogle
    haskellPackages.hlint
    haskellPackages.ghcid
    haskellPackages.stylish-haskell
    haskellPackages.hasktags
    cabal2nix
    haskellPackages.ghcide

    # Liquid Haskell
    haskellPackages.liquidhaskell
    z3
  ];

  home.file.".ghc/ghci.conf".text = with pkgs.haskellPackages; ''
    :def hoogle \s -> pure $ ":!${hoogle}/bin/hoogle --count=15 \"" <> s <> "\""
    :def pf \s -> pure $ ":!${pointfree}/bin/pointfree \"" <> s <> "\""
    :def hlint \s -> pure $ ":!${hlint}/bin/hlint \"" <> s <> "\""
    :set prompt "λ> "
    :set +t
  '';

  # TODO: investigate using ghcide / language server.
  programs.emacs.init.usePackage = {
    haskell-mode = {
      enable = true;
      after = [ "evil-leader" ];
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
      '';

      config = ''
        (evil-leader/set-key-for-mode 'haskell-mode
          "c," 'haskell-mode-format-imports
          "cb" 'haskell-interactive-switch
          "cc" 'haskell-process-cabal-build
          "cef" 'haskell-goto-first-error
          "cen" 'haskell-goto-next-error
          "cep" 'haskell-goto-prev-error
          "cl" 'haskell-process-load-file
          "cL" 'haskell-process-load-file-choose-type
          "ck" 'haskell-interactive-mode-clear
          "cr" 'haskell-process-reload
          "cs" 'haskell-mode-stylish-buffer
          "ct" 'haskell-process-do-type
          "cv" 'haskell-cabal-visit-file
          "cx" 'haskell-process-cabal)

        (evil-leader/set-key-for-mode 'haskell-interactive-mode
          "cc" 'haskell-process-interrupt
          "cf" 'next-error-follow-minor-mode
          "ck" 'haskell-interactive-mode-clear
          "cn" 'haskell-interactive-mode-prompt-next
          "cp" 'haskell-interactive-mode-prompt-previous
          "cz" 'haskell-interactive-switch-back)
      '';
    };

    liquid-types = {
      enable = true;
    };

    lsp-mode = {
      enable = true;
      after = [ "evil-leader" ];
      command = [ "lsp" ];
      config = ''
        (evil-leader/set-key
          "ll" 'lsp)
      '';
    };

    lsp-ui = {
      enable = true;
      after = [ "evil-leader" ];
      command = [ "lsp-ui-mode" ];
      init = ''
        (setq lsp-ui-doc-enable nil)
        (setq lsp-ui-doc-position 'top)
      '';

      config = ''
        (evil-leader/set-key
          "ld" 'lsp-ui-doc-glance
          "lgd" 'lsp-ui-peek-find-definitions
          "lm" 'lsp-ui-imenu)
      '';
    };

    lsp-haskell = {
      enable = true;
      init = ''
        (setq lsp-haskell-process-path-hie "ghcide")
        (setq lsp-haskell-process-args-hie '())
      '';
    };
  };
}
