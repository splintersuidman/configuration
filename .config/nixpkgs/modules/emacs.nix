{ pkgs, config, ... }:
{
  programs.emacs = {
    enable = true;
    package = pkgs.unstable.emacs;
    extraPackages = epkgs: with epkgs;
      [
        use-package

        evil
        org-evil
        evil-org
        evil-surround
        evil-numbers
        evil-leader
        evil-collection
        evil-magit
        org-alert

        which-key
        company
        irony
        company-irony
        flycheck
        flycheck-irony
        ido-vertical-mode
        avy
        switch-window
        # treemacs
        # treemacs-evil
        # expand-region
        spaceline
        feebleline
        yasnippet
        highlight-numbers
        fill-column-indicator
        magit
        magit-todos
        git-gutter
        # mingus
        sane-term
        pkgs.mu
        # notmuch
        # multiple-cursors
        pdf-tools
        # emms
        zetteldeft
        sicp
        xkcd
        direnv
        ledger-mode

        # lsp-mode
        # lsp-ui
        # company-lsp

        # Lisp
        slime
        # Haskell
        haskell-mode
        # lsp-haskell
        hindent
        # Agda
        pkgs.haskellPackages.Agda
        # Idris
        idris-mode
        # Scheme
        geiser
        # Coq
        # proof-general
        # company-coq
        # OCaml
        tuareg
        # Standard ML
        sml-mode
        # Org-mode
        org
        htmlize
        org-ref
        # LaTeX
        # tex
        auctex
        # Racket
        racket-mode
        # Rust
        rust-mode
        # Nix
        nix-mode
        # YAML
        yaml-mode
        # Go
        # go-mode
        # Zig
        zig-mode
        # Bison
        bison-mode

        # Theme
        # gruvbox-theme
        # dracula-theme
        # spacemacs-theme
        base16-theme
      ];
  };
  services.emacs.enable = true;
}
