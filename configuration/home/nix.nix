{ pkgs, ... }:
let sources = import ../../nix/sources.nix;
in {
  home.packages = [
    pkgs.nix-prefetch-git
    pkgs.cachix
    pkgs.haskellPackages.nixfmt
    pkgs.niv
    pkgs.nix-doc
  ];

  xdg.configFile."nix/nix.conf".text = ''
    plugin-files = ${pkgs.nix-doc}/lib/libnix_doc_plugin.so
  '';

  home.sessionVariables = {
    NIX_PATH = "nixpkgs=${sources.nixpkgs}:$NIX_PATH";
  };

  programs.emacs.init.usePackage = {
    nix-mode = {
      enable = true;
      after = [ "evil-leader" ];
      config = ''
        (evil-leader/set-key-for-mode 'nix-mode
          "cc" 'nix-build
          "cf" 'nix-format-buffer
          "cr" 'nix-repl)
      '';
    };
  };
}
