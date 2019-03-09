let pkgs = import <nixpkgs> {};
in with pkgs;
let
  my-python-packages = python-packages: with python-packages; [
    neovim
    requests
  ];
  python-with-my-packages = python3.withPackages my-python-packages;
in
  python-with-my-packages
