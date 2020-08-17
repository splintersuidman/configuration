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
    ./emacs/evil.nix
    ./emacs/flycheck.nix
    ./emacs/font.nix
    ./emacs/gui.nix
    ./emacs/ido.nix
    ./emacs/lsp.nix
    ./emacs/modeline.nix
    ./emacs/pairs.nix
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
    EDITOR = "${config.programs.emacs.package}/bin/emacsclient";
    VISUAL = EDITOR;
  };
}
