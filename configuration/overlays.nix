# Nixpkgs configuration. To use it system-wide, this can be imported
# in ~/.config/nixpkgs/overlays.nix.
let sources = import ../nix/sources.nix;
in [
  (import sources.emacs-overlay)
]
