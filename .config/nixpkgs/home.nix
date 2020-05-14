{ pkgs, config, lib, ... }:
let
  nurSrc = builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz";
  nurNoPkgs = import nurSrc { };
  nurWithPkgs = pkgs: import nurSrc { inherit pkgs; };
in
{
  imports = [
    ./modules
  ] ++ lib.attrValues nurNoPkgs.repos.splintah.hmModules;

  nixpkgs.config.packageOverrides = pkgs:
    {
      unstable = import <nixos-unstable> {
        config = config.nixpkgs.config;
      };
      nur = nurWithPkgs pkgs;
      clang = pkgs.clang.overrideAttrs (attrs: {
        meta.priority = pkgs.gcc.meta.priority + 1; # lower priority than gcc
      });
    };

  nixpkgs.config.allowUnfree = true;

  home.packages =
    with pkgs;
      nur.repos.splintah.lib.flattenAttrs
        {
          application = [
            libreoffice
            anki
            wineFull
            dmenu
            musescore
            signal-desktop
            qtox
          ];

          audio = [
            flac
            cdparanoia
            nur.repos.splintah.id3

            mpc_cli
            mpdscribble

            unstable.spotify-tui
          ];

          browser = [
            torbrowser
            qutebrowser
          ];

          documentation = [
            zeal
            manpages
            stdmanpages
          ];

          editor = [
            vim
            ed
            irony-server
          ];

          files = [
            ## File managers
            ranger
            nnn
            ## Downloading/uploading
            transmission-gtk
            wget
            curl
            nur.repos.splintah.onedrive
          ];

          fonts = [
            fantasque-sans-mono
            fira
            fira-code
            fira-mono
            font-awesome-ttf
            inconsolata
            libertine
            symbola
            dejavu_fonts
            siji
            unifont
            crimson
            ibm-plex
            mononoki
            unstable.jetbrains-mono
            unstable.cascadia-code
          ];

          games = [
            desmume
            (freeciv.override {
              gtkClient = true;
              sdlClient = false;
            })
            (openttd.overrideAttrs (attrs: {
              # Compile with XDG Base Directory support.
              configureFlags = attrs.configureFlags ++ [ "--with-xdg-basedir" ];
              buildInputs = attrs.buildInputs ++ [ libxdg_basedir ];
            }))
            scid-vs-pc
          ];

          graphics = [
            ## Viewers
            sxiv
            ## Screenshots
            scrot
            ## Editors
            gimp
            inkscape
            ## Fonts
            fontforge-gtk
          ];

          mail = [
            thunderbird
            neomutt
            mailutils
            mu
          ];

          media = [
            irssi
            rtv
            termtekst
          ];

          programming = {
            buildSystems = [ gnumake ];
            ides = [ arduino ];
            misc = [ radare2 radare2-cutter nix-prefetch-git ];

            agda = [ haskellPackages.Agda AgdaStdlib ];
            c = [
              gcc
              clang
              tinycc
              binutils
              indent

              bison
              flex
            ];
            coq = [ coq ];
            haskell = [
              ghc
              cabal-install
              haskellPackages.hindent
              haskellPackages.pointfree
              haskellPackages.hoogle
              haskellPackages.hlint
              haskellPackages.ghcid
              haskellPackages.stylish-haskell
              haskellPackages.hasktags
              cabal2nix
              # (let all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {}; in
              #   all-hies.selection { selector = p: { inherit (p) ghc865 ghc864; }; }
              # )
            ];
            idris = [ idris ];
            lisp = [ sbcl ];
            nix = [ haskellPackages.nixfmt ];
            ocaml = [
              ocaml
              ocamlPackages.utop
              gnum4
            ];
            octave = [ octaveFull ];
            pascal = [ fpc ];
            python = [
              python2
              python3
              python37Packages.pip
            ];
            racket = [ racket ];
            rust = [
              rustup
              rustracer
            ];
            scheme = [ guile ];
            standardML = [ smlnj ];
            zig = [ unstable.zig ];
          };

          system = [
            aspell aspellDicts.en aspellDicts.nl
            calc
            cmatrix
            coreutils
            entr
            fd
            file
            gnupg
            gnutar
            gtypist
            hplip
            intel-gpu-tools
            libGL
            libnotify
            lshw
            neofetch
            openssh
            openssl
            patchelf
            pkg-config
            qrencode
            ripgrep
            tokei
            tree
            xcape
            xclip
            xdo
            xdotool
            xfontsel
            xorg.xev
            xz
            zip unzip
          ];

          typesetting = [
            pandoc
            groff
          ];

          video = [
            ## Viewing
            vlc
            ## Downloading
            youtubeDL
            ## Editing
            ffmpeg
          ];
        };

  programs.home-manager.enable = true;

  fonts.fontconfig.enable = true;

  programs.feh.enable = true;
  programs.htop.enable = true;

  programs.opam = {
    enable = true;
    enableBashIntegration = true;
  };

  home.sessionVariables = rec {
    EDITOR = "emacsclient -c";
    VISUAL = EDITOR;

    OPAMROOT = config.xdg.dataHome + "/opam";
    LESSHISTFILE = config.xdg.dataHome + "/lesshst";
    OCTAVE_HISTFILE = config.xdg.dataHome + "/octave_hist";
    WINEPREFIX = config.xdg.dataHome + "/wine";
  };
}
