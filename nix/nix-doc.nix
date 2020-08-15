let
  sources = import ./sources.nix;
in
{ rustPlatform, nix, boost, pkg-config, stdenv }:
rustPlatform.buildRustPackage rec {
  pname = "nix-doc";
  version = "0.3.3";

  src = sources.nix-doc;

  doCheck = true;
  buildInputs = [ boost nix ];

  nativeBuildInputs = [ pkg-config ];

  cargoSha256 = "0w8xnv497niwqv6gm0n49kd795p493312kilcbi0cqmm0v49zryz";

  meta = with stdenv.lib; {
    description = "An interactive Nix documentation tool";
    longDescription = "An interactive Nix documentation tool providing a CLI for function search and a Nix plugin for docs in the REPL";
    homepage = "https://github.com/lf-/nix-doc";
    license = licenses.lgpl3;
    maintainers = [ maintainers.lf- ];
    platforms = platforms.unix;
  };
}
