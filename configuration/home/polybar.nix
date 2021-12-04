{ pkgs, config, ... }:
let colors = config.theme.base16.colors;
in {
  home.packages = [ pkgs.material-icons ];

  services.polybar = {
    enable = true;
    package = pkgs.polybar;
    script = "${config.services.polybar.package}/bin/polybar splintah &";
    config = {
      settings = { screenchange-reload = true; };

      "global/wm" = {
        margin-top = 0;
        margin-bottom = 2;
      };

      "bar/splintah" = {
        width = "100%";
        height = 19;
        radius = 0;
        fixed-center = false;

        background = "#${colors.base00.hex.rgb}";
        foreground = "#${colors.base05.hex.rgb}";

        line-size = 0;
        line-color = "#${colors.base00.hex.rgb}";
        padding-left = 1;
        padding-right = 1;

        module-margin-left = 1;
        module-margin-right = 1;

        modules-left = [ "xmonad" "window" ];
        modules-center = [ ];
        modules-right =
          [ "alsa" "xkeyboard" "memory" "cpu" "wlan" "battery" "date" ];

        font-0 =
          "Iosevka Custom:style=Medium:pixelsize=10:antialias=true:autohint=true";
        font-1 =
          "Iosevka Custom:style=Bold:pixelsize=10:antialias=true:autohint=true";
        font-2 = "Iosevka Aile:pixelsize=10:antialias=true:autohint=true";
        font-3 = "Material Icons:pixelsize=12;2";

        tray-position = "none";
        tray-padding = 2;

        cursor-click = "pointer";
        cursor-scroll = "ns-resize";

        override-redirect = true;

        enable-ipc = true;
      };

      "module/xmonad" = {
        type = "custom/script";
        exec = "${pkgs.coreutils}/bin/tail -f /tmp/.xmonad-log";
        exec-if = "[ -p /tmp/.xmonad-log ]";
        tail = true;
      };

      "module/xkeyboard" = {
        type = "internal/xkeyboard";
        blacklist-0 = "num lock";

        format-prefix =
          "%{A1:${config.home.homeDirectory}/.local/bin/switch-keyboard.sh:} ";
        format-suffix = "%{A}";
        format-prefix-foreground = "#${colors.base0B.hex.rgb}";

        label-layout = "%layout%";

        label-indicator-padding = 2;
        label-indicator-margin = 1;
        label-indicator-background = "#${colors.base0C.hex.rgb}";
      };

      "module/mpd" = {
        type = "internal/mpd";
        format-online =
          "<toggle> %{A1:${config.programs.urxvt.package}/bin/urxvt -e ${pkgs.ncmpcpp}/ bin/ncmpcpp:}<label-song>%{A}";

        icon-play = "";
        icon-pause = "";

        label-song = "%title% - %artist%";
        label-song-maxlen = 35;
        label-song-ellipsis = true;

        host = "127.0.0.1";
        port = 6600;

        interval = 5;
      };

      "module/cpu" = {
        type = "internal/cpu";
        interval = 2;
        format-prefix =
          "%{A1:${config.programs.urxvt.package}/bin/urxvt -e ${pkgs.htop}/bin/htop --sort-key PERCENT_CPU:} ";
        format-suffix = "%{A}";
        format-prefix-foreground = "#${colors.base08.hex.rgb}";
        label = "%percentage:2%%";
      };

      "module/memory" = {
        type = "internal/memory";
        interval = 2;
        format-prefix =
          "%{A1:${config.programs.urxvt.package}/bin/urxvt -e ${pkgs.htop}/bin/htop --sort-key PERCENT_MEM:} ";
        format-suffix = "%{A}";
        format-prefix-foreground = "#${colors.base0E.hex.rgb}";
        label = "%percentage_used%%";
      };

      "module/wlan" = {
        type = "internal/network";
        interval = "3.0";

        format-connected = "<label-connected>";
        label-connected = "%essid%";
        format-connected-prefix =
          "%{A1:${config.programs.urxvt.package}/bin/urxvt -e ${pkgs.networkmanager}/bin/nmtui:} ";
        format-connected-prefix-foreground = "#${colors.base0C.hex.rgb}";
        format-connected-suffix = "%{A}";

        format-disconnected = "";
        format-disconnected-foreground = "#${colors.base08.hex.rgb}";
      };

      "module/eth" = {
        type = "internal/network";
        interval = "3.0";

        format-connected-prefix = " ";
        format-connected-prefix-foreground = "#${colors.base0E.hex.rgb}";
        label-connected = "%local_ip%";

        format-disconnected = "";
      };

      "module/date" = {
        type = "internal/date";
        interval = 5;

        date = "";
        date-alt = "%Y-%m-%d";

        time = "%H:%M";
        time-alt = " %H:%M:%S";

        format-prefix = " ";
        format-prefix-foreground = "#${colors.base0D.hex.rgb}";

        label = "%date%%time%";
      };

      "module/alsa" = {
        type = "internal/alsa";

        format-volume = "<ramp-volume> <bar-volume>";
        ramp-volume-0 = "";
        ramp-volume-1 = "";
        ramp-volume-2 = "";
        ramp-volume-foreground = "#${colors.base0D.hex.rgb}";

        format-muted-prefix = " ";
        format-muted-foreground = "#${colors.base08.hex.rgb}";
        label-muted = "";

        bar-volume-width = 8;
        bar-volume-foreground-0 = "#${colors.base0B.hex.rgb}";
        bar-volume-foreground-1 = "#${colors.base0B.hex.rgb}";
        bar-volume-foreground-2 = "#${colors.base0B.hex.rgb}";
        bar-volume-foreground-3 = "#${colors.base0B.hex.rgb}";
        bar-volume-foreground-4 = "#${colors.base0B.hex.rgb}";
        bar-volume-foreground-5 = "#${colors.base0A.hex.rgb}";
        bar-volume-foreground-6 = "#${colors.base08.hex.rgb}";
        bar-volume-gradient = false;
        bar-volume-indicator = "|";
        bar-volume-indicator-font = 2;
        bar-volume-fill = "─";
        bar-volume-fill-font = 2;
        bar-volume-empty = "─";
        bar-volume-empty-font = 2;
        bar-volume-empty-foreground = "#${colors.base05.hex.rgb}";
      };

      "module/battery" = {
        type = "internal/battery";

        format-charging = "<label-charging>";
        format-charging-prefix = " ";
        format-charging-prefix-foreground = "#${colors.base0B.hex.rgb}";

        format-discharging = "<label-discharging>";
        format-discharging-prefix = " ";
        format-discharging-prefix-foreground = "#${colors.base0A.hex.rgb}";

        format-full-prefix = " ";
        format-full-prefix-foreground = "#${colors.base0B.hex.rgb}";
      };

      # NOTE: missing with respect to logging with XMonad:
      # - filter out NSP
      # - filter out workspaces without windows, not really necessary
      "module/workspaces" = {
        type = "internal/xworkspaces";

        enable-click = true;
        enable-scroll = true;

        format = "<label-state>";

        label-active = "%name%";
        label-active-foreground = "#${colors.base00.hex.rgb}";
        label-active-background = "#${colors.base0C.hex.rgb}";
        label-active-padding = 1;

        label-occupied = "%name%";
        label-occupied-padding = 1;

        label-urgent = "%name%";
        label-urgent-foreground = "#${colors.base00.hex.rgb}";
        label-urgent-background = "#${colors.base07.hex.rgb}";
        label-urgent-padding = 1;

        label-empty = "%name%";
        label-empty-padding = 1;
      };

      "module/window" = {
        type = "internal/xwindow";

        format = "<label>";
        format-foreground = "#${colors.base07.hex.rgb}";
        label = "%title%";
      };
    };
  };
}
