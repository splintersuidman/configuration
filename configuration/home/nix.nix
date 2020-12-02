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
      after = [ "general" ];
      config = ''
        (general-define-key
          :prefix my-local-leader
          :states '(normal visual motion)
          :keymaps 'nix-mode-map
          "c" '(nix-build :which-key "Build")
          "f" '(nix-format-buffer :which-key "Format buffer")
          "r" '(nix-repl :which-key "REPL"))
      '';
    };
  };
}
