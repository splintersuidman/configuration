{ pkgs, ... }:
let callPackage = pkgs.lib.callPackageWith pkgs; in
{
  id3 = callPackage ./id3 { };
  mopidy-podcast = callPackage ./mopidy-podcast { };
  ocamlweb = callPackage ./ocamlweb { };
  onedrive = callPackage ./onedrive { };
  wasm-pack = callPackage ./wasm-pack { };
}
