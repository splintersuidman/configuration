{ pkgs, inputs, ... }: {
  home.packages = [ pkgs.gnuplot ];

  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: { inherit (tpkgs) scheme-full; };
  };

  programs.emacs = {
    overrides = self: super: {
      math-delimiters = self.trivialBuild {
        pname = "math-delimiters";
        src = inputs.math-delimiters;
        version = "0";
      };
    };

    init.modules."init/init-tex.el" = {
      enable = true;
      config = ./tex.el;
      feature = "init-tex";
      extraPackages = epkgs: [ epkgs.math-delimiters ];
    };
  };
}
