# TODO: early init
{ config, lib, pkgs, inputs, ... }:
with lib;
let
  cfg = config.programs.emacs.init;
  parse = import "${inputs.emacs-overlay}/parse.nix" { inherit lib pkgs; };
in {
  options.programs.emacs.init = let
    packagesType = mkOptionType {
      name = "packages";
      description = "function from epkgs to listOf package";
      check = isFunction;
      merge = mergeOneOption;
    };

    moduleType = { name, ... }: {
      options = {
        enable = mkEnableOption "Emacs module ${name}";

        name = mkOption {
          type = types.str;
          readOnly = true;
          description = ''
            Name of the module.
          '';
        };

        config = mkOption {
          type = with types; either path lines;
          default = "";
          description = ''
            Configuration for this module. Either a path to a file that contains
            the configuration or a string that contains the configuration.
          '';
        };

        configText = mkOption {
          type = types.str;
          readOnly = true;
          internal = true;
          description = ''
            Configuration text for this module. This value is computed from
            config.
          '';
        };

        extraPackages = mkOption {
          type = packagesType;
          default = _: [ ];
          description = ''
            Extra Emacs packages to install.
          '';
        };

        extraHomePackages = mkOption {
          type = types.listOf types.package;
          default = [ ];
          description = ''
            Extra home packages to install. Specifying packages in
            extraHomePackages is equivalent to adding them to
            home.packages, but they are only added if the module is
            enabled.
          '';
        };

        packages = mkOption {
          type = packagesType;
          readOnly = true;
          internal = true;
          description = ''
            Computed packages for this module.
          '';
        };

        feature = mkOption {
          type = with types; nullOr str;
          default = null;
          description = ''
            The feature to require for this module. The feature should be
            provided by the module.
          '';
        };
      };

      config = let
        moduleCfg = cfg.modules.${name};
        inherit (moduleCfg) config extraPackages;

        configText = let type = builtins.typeOf config;
        in if type == "string" then
          config
        else if type == "path" then
          builtins.readFile config
        else
          throw ''
            Invalid for programs.emacs.init.modules."${name}".config: expected ‘path’ or ‘str’, got ‘${type}’'';
      in {
        name = mkDefault name;

        inherit configText;

        packages = epkgs:
          let
            usePackageNames = parse.parsePackagesFromUsePackage {
              inherit configText;
              alwaysEnsure = cfg.alwaysEnsure;

              # TODO: support org-mode files
              isOrgModeFile = false;
              alwaysTangle = false;
            };
            getPackageFromName = name:
              let notFound = "Cannot find Emacs pacakge for ‘${name}’";
              in epkgs.${name} or (trace notFound null);
            usePackages = map getPackageFromName usePackageNames;
          in usePackages ++ extraPackages epkgs;
      };
    };
  in {
    enable = mkEnableOption "Emacs init configuration";

    alwaysEnsure = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to interpret use-package declarations as always containing
        `:ensure t`.
      '';
    };

    byteCompile = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Compile configuration using byte compilation.
      '';
    };

    nativeCompile = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Compile configuration using native compilation.
      '';
    };

    emacsDir = mkOption {
      type = types.str;
      default = ".emacs.d";
      description = ''
        Directory of Emacs configuration, relative to $HOME.
      '';
    };

    modules = mkOption {
      type = with types; attrsOf (submodule moduleType);
      default = { };
      description = ''
        Attribute-set of Emacs init modules.
      '';
    };

    featureFile = mkOption {
      type = with types; nullOr str;
      default = null;
      description = ''
        Name of file relative to emacsDir to write the list of features of
        enabled modules to.
      '';
    };
  };

  config = let
    emacs = config.programs.emacs.finalPackage;

    enabledModules = filterAttrs (_: module: module.enable) cfg.modules;

    moduleFiles =
      mapAttrs (name: module: pkgs.writeTextDir name module.configText)
      enabledModules;

    featureFile = let
      features = map (module: module.feature)
        (filter (module: module.enable && module.feature != null)
          (attrValues cfg.modules));
    in pkgs.writeTextDir cfg.featureFile ''
      (${concatStringsSep " " features})
    '';
    maybeFeatureFile = if cfg.featureFile == null then [ ] else [ featureFile ];

    emacsDirDerivation = pkgs.symlinkJoin {
      name = cfg.emacsDir;
      paths = attrValues moduleFiles ++ maybeFeatureFile;
    };

    # The Emacs directory under $HOME.
    emacsDirHome = "${config.home.homeDirectory}/${cfg.emacsDir}";

    # Command to compile the Emacs configuration directory. ‘mode’ must be one
    # of "byte" and "native".
    # TODO: specify location/module of init.el file to load.
    compile = mode: files:
      "${emacs}/bin/emacs --quick --no-window-system --load ${emacsDirHome}/init.el -batch --funcall batch-${mode}-compile ${
        concatStringsSep " " files
      }";

    homePackages = concatMap (module: module.extraHomePackages) (attrValues enabledModules);
  in {
    programs.emacs.extraPackages = epkgs:
      concatMap (module: module.packages epkgs) (attrValues enabledModules);

    home.packages = homePackages;

    home.file.${cfg.emacsDir} = {
      recursive = true;
      source = emacsDirDerivation.outPath;
      onChange = let
        # TODO: add .el if it does not already contain .el.
        files = map (name: "${emacsDirHome}/${name}") (attrNames enabledModules);
      in ''
        # Remove .elc files. Supply -f to ignore non-existent files.
        rm -f ${concatStringsSep " " (map (name: "${name}c") files)}

        ${if cfg.byteCompile then compile "byte" files else ""}
        ${if cfg.nativeCompile then compile "native" files else ""}
      '';
    };
  };
}
