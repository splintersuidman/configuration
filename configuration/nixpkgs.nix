# Nixpkgs configuration. To use it system-wide, this can be imported
# in ~/.config/nixpkgs/config.nix.
{
  allowUnfree = true;

  packageOverrides = pkgs: {
    clang = pkgs.clang.overrideAttrs (attrs: {
      # Lower priority than gcc.
      meta.priority = pkgs.gcc.meta.priority + 1;
    });
  };
}
