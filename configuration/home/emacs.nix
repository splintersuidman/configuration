{ pkgs, config, inputs, ... }: {
  imports = [ ../../modules/home/emacs-init.nix ];

  services.emacs = {
    enable = true;
    client = {
      enable = true;
      arguments = [ "--create-frame" ];
    };
    socketActivation.enable = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsUnstable;

    overrides = self: super: {
      lin = self.trivialBuild {
        pname = "lin";
        src = inputs.lin;
      };

      ligature = self.trivialBuild {
        pname = "ligature";
        src = inputs."ligature.el";
      };
    };

    init = {
      enable = true;
      alwaysEnsure = false;
      byteCompile = true;
      nativeCompile = true;
      emacsDir = ".emacs.d";
      featureFile = "init-features";
      modules = {
        "init.el" = {
          enable = true;
          config = ./emacs/init.el;
        };

        "init/init-completion.el" = {
          enable = true;
          config = ./emacs/init/completion.el;
          feature = "init-completion";
          # extraPackages = epkgs: [ epkgs.vertico-buffer ];
        };

        "init/init-desktop.el" = {
          enable = true;
          config = ./emacs/init/desktop.el;
          feature = "init-desktop";
        };

        "init/init-eglot.el" = {
          enable = true;
          config = ./emacs/init/eglot.el;
          feature = "init-eglot";
        };

        "init/init-eshell.el" = {
          enable = true;
          config = ./emacs/init/eshell.el;
          feature = "init-eshell";
        };

        "init/init-files.el" = {
          enable = true;
          config = ./emacs/init/files.el;
          feature = "init-files";
        };

        "init/init-flymake.el" = {
          enable = true;
          config = ./emacs/init/flymake.el;
          feature = "init-flymake";
        };

        "init/init-fold.el" = {
          enable = true;
          config = ./emacs/init/fold.el;
          feature = "init-fold";
        };

        "init/init-font.el" = {
          enable = true;
          config = ./emacs/init/font.el;
          feature = "init-font";
        };

        "init/init-frame.el" = {
          enable = true;
          config = ./emacs/init/frame.el;
          feature = "init-frame";
        };

        "init/init-gui.el" = {
          enable = true;
          config = ./emacs/init/gui.el;
          feature = "init-gui";
          extraPackages = epkgs: [ epkgs.ligature epkgs.lin ];
        };

        "init/init-help.el" = {
          enable = true;
          config = ./emacs/init/help.el;
          feature = "init-help";
        };

        "init/init-icons.el" = {
          enable = true;
          config = ./emacs/init/icons.el;
          feature = "init-icons";
        };

        "init/init-imenu.el" = {
          enable = true;
          config = ./emacs/init/imenu.el;
          feature = "init-imenu";
        };

        "init/init-indent.el" = {
          enable = true;
          config = ./emacs/init/indent.el;
          feature = "init-indent";
        };

        "init/init-keybindings.el" = {
          enable = true;
          config = ./emacs/init/keybindings.el;
          feature = "init-keybindings";
        };

        "init/init-modeline.el" = {
          enable = true;
          config = ./emacs/init/modeline.el;
          feature = "init-modeline";
        };

        "init/init-pairs.el" = {
          enable = true;
          config = ./emacs/init/pairs.el;
          feature = "init-pairs";
        };

        "init/init-project.el" = {
          enable = true;
          config = ./emacs/init/project.el;
          feature = "init-project";
        };

        "init/init-scratch.el" = {
          enable = true;
          config = ./emacs/init/scratch.el;
          feature = "init-scratch";
        };

        "init/init-tab-bar.el" = {
          enable = true;
          config = ./emacs/init/tab-bar.el;
          feature = "init-tab-bar";
        };

        "init/init-theme.el" = {
          enable = true;
          config = ./emacs/init/theme.el;
          feature = "init-theme";
        };

        "init/init-window.el" = {
          enable = true;
          config = ./emacs/init/window.el;
          feature = "init-window";
        };
      };
    };
  };

  home.sessionVariables = rec {
    EDITOR = "${config.programs.emacs.package}/bin/emacsclient -c";
    VISUAL = EDITOR;
  };
}
