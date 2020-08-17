{ pkgs, lib, ... }:
let
  pylsWithPackages = pythonPackages: extraPackages:
    let
      pyls = pythonPackages.python-language-server;
    in
    pkgs.buildEnv {
      name = "pyls-with-packages-${pyls.version}";
      paths = lib.closePropagation (extraPackages pythonPackages);
      pathsToLink = [ "/${pythonPackages.python.sitePackages}" ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        makeWrapper ${pyls}/bin/pyls $out/bin/pyls \
          --prefix PYTHONPATH : $out/${pythonPackages.python.sitePackages}
      '';
    };

  pyls = pylsWithPackages pkgs.python3Packages (pyPkgs: [
    pyPkgs.pyls-black
    pyPkgs.pyls-mypy
  ]);
in
{
  home.packages = [
    pkgs.python3
    pyls
  ];
}
