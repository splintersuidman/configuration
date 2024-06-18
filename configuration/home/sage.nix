{ pkgs, ... }: {
  home.packages = [
    (pkgs.sage.override {
      requireSageTests = false;
      extraPythonPackages = ps: with ps; [ notebook ];
    })
  ];

  programs.emacs.init.modules."init/init-sage.el" = {
    enable = true;
    config = ./sage.el;
    feature = "init-sage";
  };
}
