# Nixpkgs configuration. To use it system-wide, this can be imported
# in ~/.config/nixpkgs/config.nix.
let
  sources = import ../nix/sources.nix;
in
{
  allowUnfree = true;

  packageOverrides = pkgs: {
    unstable = import <nixos-unstable> { };

    nur = import sources.nur { inherit pkgs; };

    clang = pkgs.clang.overrideAttrs (attrs: {
      # Lower priority than gcc.
      meta.priority = pkgs.gcc.meta.priority + 1;
    });
  };
}
