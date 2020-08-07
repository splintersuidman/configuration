let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs { };
  passenv-lib = pkgs.haskellPackages.callPackage "${sources.passenv}/passenv.nix" { };
in
{
  lib.getSecret = name:
    let result = builtins.getEnv name; in
    if result == ""
      then builtins.throw ("Environment variable " + name + " not defined.")
      else result;

  pkg = pkgs.stdenv.mkDerivation {
    pname = "passenv";
    version = "0";

    buildInputs = [
      (pkgs.haskellPackages.ghcWithPackages (hpkgs: [
        passenv-lib
        hpkgs.bytestring
        hpkgs.pureMD5
      ]))
    ];

    # Do not do unpack phase, because unpacking a single file does not
    # work.
    phases = [ "buildPhase" "installPhase" ];

    buildPhase = ''
      ghc -O -Wall $src -o passenv
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp passenv $out/bin/passenv
    '';

    src = pkgs.writeText "passenv.hs" ''
      {-# LANGUAGE OverloadedStrings #-}

      import Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
      import Data.Digest.Pure.MD5 (md5)
      import Passenv

      md5Sum :: String -> String
      md5Sum = show . md5 . ByteString.Lazy.Char8.pack

      main :: IO ()
      main = passenv def
        $ passKV "libre.fm" (env
            [ ("LIBREFM_USERNAME", value "username")
            , ("LIBREFM_PASSWORD", md5Sum <$> password)
            ])
       <> passKV "spotify.com" (env
            [ ("SPOTIFY_PASSWORD", password)
            , ("SPOTIFY_USERNAME", value "username")
            , ("SPOTIFY_CLIENT_ID", value "client_id")
            , ("SPOTIFY_CLIENT_SECRET", value "client_secret")
            ])
    '';
  };
}
