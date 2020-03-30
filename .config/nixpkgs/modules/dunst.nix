{ pkgs, config, ... }:
let
  colours = config.colours;
  firefox = config.programs.firefox.package;
in
{
  services.dunst = {
    enable = true;
    settings = {
      global = {
        monitor = 0;
        follow = "keyboard";
        geometry = "300x5-30+20";
        indicate_hidden = true;
        shrink = false;
        transparency = 0;
        separator_height = 2;
        padding = 8;
        horizontal_padding = 8;
        frame_width = 3;
        frame_color = colours.background1;
        separator_color = "frame";
        sort = true;
        idle_threshold = 120;
        font = "Dejavu Sans Mono 9";
        line_height = 0;
        markup = "full";
        format = "<b>%s</b>\n%b";
        alignment = "left";
        show_age_threshold = 60;
        word_wrap = true;
        ellipsize = "middle";
        ignore_newline = false;
        stack_duplicates = true;
        hide_duplicate_count = false;
        show_indicators = true;
        icon_position = "off";
        max_icon_size = 32;
        sticky_history = true;
        history_length = 20;
        dmenu = "${pkgs.dmenu}/bin/dmenu -p dunst:";
        browser = "${firefox}/bin/firefox -new-tab";
        always_run_script = true;
        title = "Dunst";
        class = "Dunst";
        startup_notification = false;
        force_xinerama = false;
      };
      shortcuts = {
        close = "ctrl+space";
        close_all = "ctrl+shift+space";
        history = "ctrl+grave";
        context = "ctrl+shift+period";
      };
      urgency_low = {
        background = colours.background;
        foreground = colours.foreground;
        frame_color = colours.foreground0;
        timeout = 10;
      };
      urgency_normal = {
        background = colours.background;
        foreground = colours.foreground;
        timeout = 10;
      };
      urgency_critical = {
        background = colours.background;
        foreground = colours.foreground4;
        frame_color = colours.red;
        timeout = 0;
      };
    };
  };
}
