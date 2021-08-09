{ config, pkgs, ... }:
let
  colors = config.theme.base16.colors;
  background = "#${colors.base00.hex.rgb}";
  backgroundAlt = "#${colors.base00.hex.rgb}";
  foreground = "#${colors.base05.hex.rgb}";
  foregroundAlt = "#${colors.base04.hex.rgb}";
in {
  programs.zathura = {
    enable = true;
    options = {
      selection-clipboard = "clipboard";
      font = "Iosevka Custom 10";
      page-padding = 1;
      default-bg = background;
      default-fg = foreground;
      completion-bg = background;
      completion-fg = foreground;
      completion-group-bg = background;
      completion-group-fg = foreground;
      completion-highlight-bg = background;
      completion-highlight-fg = foreground;
      inputbar-bg = background;
      inputbar-fg = foreground;
      statusbar-bg = background;
      statusbar-fg = foreground;
      recolor-darkcolor = foregroundAlt;
      recolor-lightcolor = backgroundAlt;
      recolor = false;
    };
  };

  programs.emacs.init.modules."init/init-pdf.el" = {
    enable = true;
    config = ./pdf.el;
    feature = "init-pdf";
  };
}
