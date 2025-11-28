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
    (pyPkgs: [ pyPkgs.pyls-flake8 pyPkgs.pyls-isort pyPkgs.pylsp-mypy ]);
in {
  home.packages = [
    pkgs.python3
    pkgs.python3Packages.ipython
    pkgs.python3Packages.black
    pkgs.python3Packages.isort
    pkgs.python3Packages.flake8
    pkgs.python3Packages.black-macchiato
    pylsp
    pkgs.basedpyright
    pkgs.ruff
  ];

  programs.emacs.init.modules."init/init-python.el" = {
    enable = true;
    config = ./python.el;
    feature = "init-python";
  };
}
