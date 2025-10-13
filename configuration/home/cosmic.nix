{ config, pkgs, inputs, ... }:
let cosmic = config.lib.cosmic;
in {
  programs.cosmic-applibrary.enable = true;
  programs.cosmic-edit.enable = false;
  programs.cosmic-ext-tweaks.enable = true;
  programs.cosmic-manager.enable = true;
  programs.cosmic-files = {
    enable = true;
    settings = {
      desktop = {
        show_content = true;
        show_mounted_drives = true;
        show_trash = false;
      };
      favorites = [
        (cosmic.mkRON "enum" "Home")
        (cosmic.mkRON "enum" {
          variant = "Path";
          value = [ config.xdg.userDirs.desktop ];
        })
        (cosmic.mkRON "enum" "Documents")
        (cosmic.mkRON "enum" "Downloads")
        (cosmic.mkRON "enum" "Music")
        (cosmic.mkRON "enum" "Pictures")
        (cosmic.mkRON "enum" "Videos")
        (cosmic.mkRON "enum" {
          variant = "Path";
          value = [ "/tmp" ];
        })
      ];
    };
  };
  programs.cosmic-player.enable = true;
  programs.cosmic-store.enable = false;
  programs.cosmic-term = {
    enable = true;
    settings = {
      font_name = "Iosevka Custom";
      font_size = 14;
      show_headerbar = true;
    };
    profiles = [
      {
        command = "bash";
        hold = true;
        is_default = true;
        name = "Default";
        syntax_theme_dark = "COSMIC Dark";
        syntax_theme_light = "COSMIC Light";
        working_directory = config.home.homeDirectory;
      }
    ];
  };
  programs.forecast.enable = true;

  wayland.desktopManager.cosmic = {
    enable = true;
    configFile = {
      "com.system76.CosmicAppletTime" = {
        version = 1;
        entries = {
          show_seconds = false;
          military_time = true;
          show_date_in_top_panel = true;
          first_day_of_week = 0;
        };
      };
    };
  };

  # See <https://wiki.nixos.org/wiki/COSMIC#Theming_and_Firefox>.
  programs.firefox.profiles.default.settings."widget.gtk.libadwaita-colors.enabled" = false;
}
