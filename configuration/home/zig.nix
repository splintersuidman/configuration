{ pkgs, ... }:
let zig = pkgs.unstable.zig;
in {
  home.packages = [ zig ];

  programs.emacs.init.usePackage = { zig-mode = { enable = true; }; };
}
