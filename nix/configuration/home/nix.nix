{ pkgs, ... }:
{
  home.packages = [
    pkgs.nix-prefetch-git
    pkgs.cachix
    pkgs.haskellPackages.nixfmt
    pkgs.niv
  ];

  programs.emacs.init.usePackage = {
    nix-mode = {
      enable = true;
      after = [ "evil-leader" ];
      config = ''
        (evil-leader/set-key-for-mode 'nix-mode
          "cc" 'nix-build
          "cr" 'nix-repl)
      '';
    };
  };
}
