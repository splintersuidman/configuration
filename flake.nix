{
  description = ''
    Home and NixOS configuration.

    The home configuration can be built (à la 'home-manager build') using
      nix build '.#<host>'
    and to the configuration can be switched (à la 'home-manager switch') using
      nix run '.#<host>'
    where '<host>' is the name of the host (e.g. 'thinkpad').

    The system configuration can be built using
      sudo nixos-rebuild build --flake '.#<host>'
    and to the configuration can be switched using
      sudo nixos-rebuild switch --flake '.#<host>'.
  '';

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/nur";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    base16-tomorrow-scheme = {
      url = "github:chriskempson/base16-tomorrow-scheme";
      flake = false;
    };
    nixpkgs-iosevka.url =
      "github:nixos/nixpkgs?rev=838eefb4f93f2306d4614aafb9b2375f315d917f";
    kmonad = {
      url = "github:kmonad/kmonad?dir=nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    lin = {
      url = "gitlab:protesilaos/lin";
      flake = false;
    };
    mopidy-mpd = {
      url = "github:mopidy/mopidy-mpd";
      flake = false;
    };
    xmonad-splintah = {
      url = "github:splintah/xmonad-splintah";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    private = {
      url = "path:/home/splinter/configuration/private";
      flake = false;
    };
  };

  outputs = { self, ... }@inputs:
    let
      overlays = [
        inputs.nur.overlay
        inputs.emacs-overlay.overlay
        inputs.kmonad.overlay
        (final: prev: {
          unstable = import inputs.nixpkgs-unstable {
            system = "x86_64-linux";
            config.allowUnfree = true;
          };

          # Pin Iosevka independently from Nixpkgs, since building the
          # modified version requires a lot of resources.
          iosevka = (import inputs.nixpkgs-iosevka {
            system = "x86_64-linux";
            config.allowUnfree = true;
          }).iosevka;

          xmonad-splintah = inputs.xmonad-splintah.defaultPackage.x86_64-linux;
        })
      ];
    in {
      homeConfigurations = {
        thinkpad = inputs.home-manager.lib.homeManagerConfiguration {
          pkgs = import inputs.nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
          };
          configuration = { ... }: {
            imports = let
              nurNoPkgs = import inputs.nur {
                nurpkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
              };
            in [
              ./hosts/thinkpad/home.nix
              # NOTE: these modules must be imported here to prevent infinite
              # recursion. See
              # <https://github.com/nix-community/home-manager/issues/1642#issuecomment-739012921>.
              nurNoPkgs.repos.rycee.hmModules.theme-base16
              nurNoPkgs.repos.splintah.hmModules.onedrive
              nurNoPkgs.repos.splintah.hmModules.mopidy
              nurNoPkgs.repos.splintah.hmModules.mpdscribble
              "${inputs.private}/configuration/home/mail/accounts.nix"
              "${inputs.private}/configuration/home/mopidy.nix"
              "${inputs.private}/configuration/home/mpdscribble.nix"
            ];
            nixpkgs = {
              config.allowUnfree = true;
              inherit overlays;
            };
            _module.args.inputs = inputs;
          };
          system = "x86_64-linux";
          homeDirectory = "/home/splinter";
          username = "splinter";
        };
      };

      nixosConfigurations = {
        thinkpad = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./hosts/thinkpad/system.nix
            {
              nix = {
                registry = {
                  nixpkgs.flake = inputs.nixpkgs;
                  unstable.flake = inputs.nixpkgs-unstable;
                  home-manager.flake = inputs.home-manager;
                  nur.flake = inputs.nur;
                  emacs-overlay.flake = inputs.emacs-overlay;
                  nixos-hardware.flake = inputs.nixos-hardware;
                };
                nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
              };
              nixpkgs.overlays = overlays;
            }
          ];
          specialArgs = { inherit inputs; };
        };
      };

      packages.x86_64-linux =
        builtins.mapAttrs (_name: config: config.activationPackage)
        self.homeConfigurations;

      apps.x86_64-linux = builtins.mapAttrs (name: activationPackage: {
        type = "app";
        program = "${activationPackage}/activate";
      }) self.packages.x86_64-linux;
    };
}
