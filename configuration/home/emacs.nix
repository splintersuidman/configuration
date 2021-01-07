{ pkgs, config, ... }: {
  imports = [
    ./emacs/avy.nix
    ./emacs/completion.nix
    ./emacs/elfeed.nix
    ./emacs/evil.nix
    ./emacs/flycheck.nix
    ./emacs/flymake.nix
    ./emacs/fold.nix
    ./emacs/font.nix
    ./emacs/gui.nix
    ./emacs/help.nix
    ./emacs/icons.nix
    ./emacs/ido.nix
    ./emacs/indent.nix
    ./emacs/ivy.nix
    ./emacs/jump.nix
    ./emacs/lsp.nix
    ./emacs/modeline.nix
    ./emacs/pairs.nix
    ./emacs/pdf.nix
    ./emacs/shell.nix
    ./emacs/snippets.nix
    ./emacs/switch-window.nix
    ./emacs/theme.nix
    ./emacs/vterm.nix
    ./emacs/which-key.nix
  ];

  services.emacs.enable = true;
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;

    init = {
      enable = true;
      recommendedGcSettings = true;
      # NOTE: without this option, company-mode does not seem to load, and
      # initial-scratch-message is not inserted, for example. See
      # <https://gitlab.com/rycee/nur-expressions/-/issues/23> and
      # <https://gitlab.com/rycee/nur-expressions/-/commit/23d30ea7a3b97f7734f9383f93b5ded1c08d826d>.
      earlyInit = "(setq package-enable-at-startup t)";
    };
  };

  home.sessionVariables = rec {
    EDITOR = "${config.programs.emacs.package}/bin/emacsclient -c";
    VISUAL = EDITOR;
  };
}
