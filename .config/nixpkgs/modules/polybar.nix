{ pkgs, config, ... }:
{
  services.polybar = {
    enable = true;
    package = pkgs.polybar.override { mpdSupport = true; };
    script = "${config.services.polybar.package}/bin/polybar splintah &";
    config = rec {
      colors = with config.colours; {
        inherit (config.colours) black red green yellow blue magenta cyan white;
        bright-black = brightBlack;
        bright-red = brightRed;
        bright-green  = brightGreen;
        bright-yellow = brightYellow;
        bright-blue = brightBlue;
        bright-magenta = brightMagenta;
        bright-cyan = brightCyan;
        bright-white = brightWhite;

        inherit (config.colours) background foreground;
        background-alt = background0;
        foreground-alt = foreground4;
        primary = colors.bright-blue;
        secondary = colors.bright-cyan;
        alert = colors.red;
      };

      settings = {
        screenchange-reload = true;
      };

      "global/wm" = {
        margin-top = 0;
        margin-bottom = 2;
      };

      "bar/splintah" = {
        width = "100%";
        height = 20;
        radius = 0;
        fixed-center = false;

        background = colors.background;
        foreground = colors.foreground;

        line-size = 2;
        line-color = colors.background;
        padding-left = 1;
        padding-right = 1;

        module-margin-left = 1;
        module-margin-right = 1;

        modules-left = [ "xmonad" ];
        modules-center = [ ];
        modules-right = [ "alsa" "xkeyboard" "memory" "cpu" "eth" "wlan" "battery" "date" ];

        # font-0 = "Fira Mono:pixelsize=9:antialias=true:autohint=true";
        font-0 = "DejaVu Sans Mono:pixelsize=9:antialias=true:autohint=true";
        font-1 = "DejaVu Sans Mono:style=Bold:pixelsize=9:antialias=true:autohint=true";
        font-2 = "unifont:fontformat=truetype:size=8:antialias=false;0";
        font-3 = "siji:pixelsize=10;1";

        tray-position = "right";
        tray-padding = 2;

        cursor-click = "pointer";
        cursor-scroll = "ns-resize";

        override-redirect = true;
      };

      "module/xmonad" = {
        type = "custom/script";
        exec = "${pkgs.coreutils}/bin/tail -F /tmp/.xmonad-log";
        exec-if = "[ -p /tmp/.xmonad-log ]";
        tail = true;
      };

      "module/xkeyboard" = {
        type = "internal/xkeyboard";
        blacklist-0 = "num lock";

        format-prefix = "%{A1:${config.home.homeDirectory}/scripts/switch-keyboard.sh:}%{T1}KB%{T-} ";
        format-suffix = "%{A}";
        format-prefix-foreground = colors.foreground-alt;
        # format-prefix-underline = colors.cyan;
        # format-prefix-underline = colors.green; # NEW

        label-layout = "%layout%";
        # label-layout-underline = colors.cyan;
        # label-layout-underline = colors.green; # NEW

        label-indicator-padding = 2;
        label-indicator-margin = 1;
        label-indicator-background = colors.cyan;
        # label-indicator-underline = colors.cyan;
        # label-indicator-underline = colors.green; # NEW
      };

      "module/mpd" = {
        type = "internal/mpd";
        format-online = "<toggle> %{A1:${config.programs.urxvt.package}/bin/urxvt -e ${pkgs.ncmpcpp}/ bin/ncmpcpp:}<label-song>%{A}";

        icon-play = "▶";
        icon-pause = "⏸";

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
        format-prefix = "%{A1:${config.programs.urxvt.package}/bin/urxvt -e ${pkgs.htop}/bin/htop --sort-key PERCENT_CPU:}%{T1}CPU%{T-} ";
        format-suffix = "%{A}";
        format-prefix-foreground = colors.foreground-alt;
        # format-underline = colors.red;
        # format-underline = colors.green; # NEW
        label = "%percentage:2%%";
      };

      "module/memory" = {
        type = "internal/memory";
        interval = 2;
        format-prefix = "%{A1:${config.programs.urxvt.package}/bin/urxvt -e ${pkgs.htop}/bin/htop --sort-key PERCENT_MEM:}%{T1}MEM%{T-} ";
        format-suffix = "%{A}";
        format-prefix-foreground = colors.foreground-alt;
        # format-underline = colors.bright-blue;
        # format-underline = colors.green; # NEW
        label = "%percentage_used%%";
      };

      "module/wlan" = {
        type = "internal/network";
        interface = "wlp2s0";
        interval = "3.0";

        format-connected = "<ramp-signal> <label-connected>";
        # format-connected-underline = colors.magenta;
        # format-connected-underline = colors.green; # NEW
        label-connected = "%essid%";
        format-connected-prefix =
          "%{A1:${config.programs.urxvt.package}/bin/urxvt -e ${pkgs.networkmanager}/bin/nmtui:}%{T1}";
        format-connected-suffix = "%{T-}%{A}";

        format-disconnected = "";

        ramp-signal-0 = "WLAN";
        ramp-signal-1 = "%{F${colors.bright-white}}W%{F${colors.white}}LAN";
        ramp-signal-2 = "%{F${colors.bright-white}}WL%{F${colors.white}}AN";
        ramp-signal-3 = "%{F${colors.bright-white}}WLA%{F${colors.white}}N";
        ramp-signal-4 = "%{F${colors.bright-white}}WLAN%{F${colors.white}}";
        ramp-signal-foreground = colors.foreground-alt;
      };

      "module/eth" = {
        type = "internal/network";
        interface = "enp0s25";
        interval = "3.0";

        # format-connected-underline = colors.green; # NEW
        format-connected-prefix = "ETH ";
        format-connected-prefix-foreground = colors.foreground-alt;
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

        # format-underline = colors.blue;
        # format-underline = colors.green; # NEW

        label = "%date%%time%";
      };

      "module/alsa" = {
        type = "internal/alsa";

        format-volume = "<label-volume> <bar-volume>";
        label-volume = "%{T1}VOL%{T-}";
        label-volume-foreground = colors.foreground-alt;

        format-muted-prefix = " ";
        format-muted-foreground = colors.foreground-alt;
        label-muted = "geluid uit";

        bar-volume-width = 8;
        bar-volume-foreground-0 = colors.green;
        bar-volume-foreground-1 = colors.green;
        bar-volume-foreground-2 = colors.green;
        bar-volume-foreground-3 = colors.green;
        bar-volume-foreground-4 = colors.green;
        bar-volume-foreground-5 = colors.bright-yellow;
        bar-volume-foreground-6 = colors.bright-red;
        bar-volume-gradient = false;
        bar-volume-indicator = "|";
        bar-volume-indicator-font = 2;
        bar-volume-fill = "─";
        bar-volume-fill-font = 2;
        bar-volume-empty = "─";
        bar-volume-empty-font = 2;
        bar-volume-empty-foreground = colors.foreground;
      };

      "module/battery" = {
        type = "internal/battery";
        battery = "BAT0";
        # adapter = "ADP1";
        adapter = "AC";
        full-at = 98;

        format-charging = "%{T1}<animation-charging> <label-charging>%{T-}";
        # format-charging-underline = colors.green; # NEW

        format-discharging = "%{T1}<animation-discharging> <label-discharging>%{T-}";
        # format-discharging-underline = colors.bright-yellow;
        # format-discharging-underline = colors.yellow; # NEW

        format-full-prefix = "= ";
        format-full-prefix-foreground = colors.foreground-alt;
        # format-full-underline = colors.bright-green;
        # format-full-underline = colors.green; # NEW

        ramp-capacity-0 = "";
        ramp-capacity-1 = "";
        ramp-capacity-2 = "";
        ramp-capacity-foreground = colors.foreground-alt;

        animation-charging-0 = "↑";
        animation-charging-1 = "↑";
        animation-charging-2 = "↑";
        animation-charging-foreground = colors.foreground-alt;
        animation-charging-framerate = 750;

        animation-discharging-0 = "↓";
        animation-discharging-1 = "↓";
        animation-discharging-2 = "↓";
        animation-discharging-foreground = colors.foreground-alt;
        animation-discharging-framerate = 750;
      };
    };
  };
}
