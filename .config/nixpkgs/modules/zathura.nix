{ pkgs, config, ... }:
let colours = config.colours; in
{
  programs.zathura = {
    enable = true;
    options = {
      selection-clipboard = "clipboard";
      font = "DejaVu Sans Mono 9";
      page-padding = 1;
      default-bg = colours.background1;
      default-fg = colours.foreground0;
      completion-bg = colours.background1;
      completion-fg = colours.foreground0;
      completion-group-bg = colours.background1;
      completion-group-fg = colours.foreground0;
      completion-highlight-bg = colours.foreground0;
      completion-highlight-fg = colours.background1;
      inputbar-bg = colours.background1;
      inputbar-fg = colours.foreground0;
      statusbar-bg = colours.background1;
      statusbar-fg = colours.foreground0;
      recolor-darkcolor = colours.foreground;
      recolor-lightcolor = colours.background;
      recolor = false;
    };
  };
}
