let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  niv = (import sources.niv { }).niv;
  passenv = (import ./nix/passenv.nix).pkg;
in
pkgs.mkShell {
  buildInputs = [
    niv
    passenv
  ];
}
