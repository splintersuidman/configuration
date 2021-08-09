{ pkgs, ... }:
let zig = pkgs.unstable.zig;
in {
  home.packages = [ zig ];

  programs.emacs.init.modules."init/init-zig.el" = {
    enable = true;
    config = ./zig.el;
    feature = "init-zig";
  };
}
