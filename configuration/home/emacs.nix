{ pkgs, config, ... }:
let
  sources = import ../../nix/sources.nix;
  nurNoPkgs = import sources.nur { };
in
{
  imports = [
    nurNoPkgs.repos.rycee.hmModules.emacs-init

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
    };
  };

  home.sessionVariables = rec {
    EDITOR = "${config.programs.emacs.package}/bin/emacsclient -c";
    VISUAL = EDITOR;
  };
}
