{ pkgs, lib, ... }:
let
  pylsWithPackages = pythonPackages: extraPackages:
    let pyls = pythonPackages.python-language-server;
    in pkgs.buildEnv {
      name = "pyls-with-packages-${pyls.version}";
      paths = lib.closePropagation (extraPackages pythonPackages);
      pathsToLink = [ "/${pythonPackages.python.sitePackages}" ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        makeWrapper ${pyls}/bin/pyls $out/bin/pyls \
          --prefix PYTHONPATH : $out/${pythonPackages.python.sitePackages}
      '';
    };

  pyls = pylsWithPackages pkgs.python3Packages
    (pyPkgs: [ pyPkgs.pyls-black pyPkgs.pyls-mypy ]);

  pylspWithPackages = pythonPackages: extraPackages:
    let pylsp = pythonPackages.python-lsp-server;
    in pkgs.buildEnv {
      name = "pylsp-with-packages-${pylsp.version}";
      paths = lib.closePropagation (extraPackages pythonPackages);
      pathsToLink = [ "/${pythonPackages.python.sitePackages}" ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        makeWrapper ${pylsp}/bin/pylsp $out/bin/pylsp \
          --prefix PYTHONPATH : $out/${pythonPackages.python.sitePackages}
      '';
    };

  pylsp = pylspWithPackages pkgs.python3Packages
    (pyPkgs: [ pyPkgs.pylsp-mypy pyPkgs.pyls-isort ]);
in { home.packages = [ pkgs.python3 pkgs.python3Packages.ipython pylsp ]; }
