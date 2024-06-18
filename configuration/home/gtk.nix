{ pkgs, config, ... }:
let
  colors = config.theme.base16.colors;
  rgb = base: "#${base.hex.rgb}";
in
{
  gtk = {
    enable = false;
    theme = {
      name = "Adwaita-dark";
    };
    font = {
      name = "DejaVu Sans 10";
      # name = "Note Sans 10";
    }; 

    gtk3.extraConfig = {
      # gtk-application-prefer-dark-theme = true;
    };
    # gtk3.extraCss = ''
    #   @define-color bg_color ${rgb colors.base00};
    #   @define-color fg_color ${rgb colors.base05};
    #   @define-color base_color ${rgb colors.base00};
    #   @define-color text_color ${rgb colors.base05};
    #   @define-color selected_bg_color ${rgb colors.base02};
    #   @define-color selected_fg_color ${rgb colors.base05};
    #   @define-color tooltip_bg_color ${rgb colors.base00};
    #   @define-color tooltip_fg_color ${rgb colors.base05};

    #   @define-color theme_bg_color @bg_color;
    #   @define-color theme_fg_color @fg_color;
    #   @define-color theme_base_color @base_color;
    #   @define-color theme_text_color @text_color;
    #   @define-color theme_selected_bg_color @selected_bg_color;
    #   @define-color theme_selected_fg_color @selected_fg_color;
    #   @define-color theme_tooltip_bg_color @tooltip_bg_color;
    #   @define-color theme_tooltip_fg_color @tooltip_fg_color;
    # '';
  };
}
