{ pkgs, config, ... }: {
  home.packages = with pkgs; [
    ghc
    cabal-install
    stack
    haskellPackages.pointfree
    haskellPackages.hoogle
    haskellPackages.hlint
    haskellPackages.ghcid
    haskellPackages.stylish-haskell
    haskellPackages.hasktags
    cabal2nix
    haskellPackages.ghcide
    haskellPackages.haskell-language-server
  ];

  home.file.".ghc/ghci.conf".text = with pkgs.haskellPackages; ''
    :def hoogle \s -> pure $ ":!${hoogle}/bin/hoogle --count=15 \"" <> s <> "\""
    :def hlint \s -> pure $ ":!${hlint}/bin/hlint \"" <> s <> "\""
    :def pf \s -> pure $ ":!${pointfree}/bin/pointfree \"" <> s <> "\""
    :set prompt "λ> "
    :set +t
  '';

  programs.emacs.init.modules."init/init-haskell.el" = {
    enable = true;
    config = ./haskell.el;
    feature = "init-haskell";
  };
}
