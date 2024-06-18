{ pkgs, inputs, ... }:
let
  pythonPackages = pkgs.python3Packages;
  rmirro = pythonPackages.buildPythonPackage {
    pname = "rmirro";
    version = "0";

    src = inputs.rmirro;

    doCheck = false;
  };
in {
  home.packages = [
    pkgs.unstable.rmview
    rmirro
  ];
}
