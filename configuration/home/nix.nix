{ pkgs, config, ... }: {
  home.packages = [
    pkgs.nix-prefetch-git
    pkgs.cachix
    pkgs.haskellPackages.nixfmt
    pkgs.niv
    pkgs.nil
    # pkgs.nix-doc
  ];

  # xdg.configFile."nix/nix.conf".text = ''
  #   plugin-files = ${pkgs.nix-doc}/lib/libnix_doc_plugin.so
  # '';

  programs.emacs.init.modules."init/init-nix.el" = {
    enable = true;
    config = ./nix.el;
    feature = "init-nix";
  };
}
