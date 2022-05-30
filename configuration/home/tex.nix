{ pkgs, ... }: {
  home.packages = [ pkgs.gnuplot ];

  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: { inherit (tpkgs) scheme-full; };
  };

  programs.emacs.init.modules."init/init-tex.el" = {
    enable = true;
    config = ./tex.el;
    feature = "init-tex";
  };
}
