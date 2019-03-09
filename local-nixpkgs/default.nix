with (import <nixpkgs> {}); {
  ranger = callPackage ./ranger {};
  # my-python = callPackage ./my-python {};
}
