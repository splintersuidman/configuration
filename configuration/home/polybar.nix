{ pkgs, config, ... }:
let
  colors = config.theme.base16.colors;
in
{
  services.polybar = {
    enable = true;
    package = pkgs.polybar.override { mpdSupport = true; };
    script = "${config.services.polybar.package}/bin/polybar splintah &";
    config = {
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

        background = "#${colors.base00.hex.rgb}";
        foreground = "#${colors.base05.hex.rgb}";

        line-size = 2;
        line-color = "#${colors.base00.hex.rgb}";
        padding-left = 1;
        padding-right = 1;

        module-margin-left = 1;
        module-margin-right = 1;

        modules-left = [ "xmonad" ];
        modules-center = [ ];
        modules-right = [ "alsa" "xkeyboard" "memory" "cpu" "eth" "wlan" "battery" "date" ];

        font-0 = "Iosevka Custom:pixelsize=10:antialias=true:autohint=true";
        font-1 = "Iosevka Custom:style=Bold:pixelsize=10:antialias=true:autohint=true";
        font-2 = "unifont:fontformat=truetype:size=8:antialias=false;0";
        font-3 = "siji:pixelsize=10;1";

        tray-position = "none";
        tray-padding = 2;

        cursor-click = "pointer";
        cursor-scroll = "ns-resize";

        override-redirect = true;

        enable-ipc = true;
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

        format-prefix = "%{A1:${config.home.homeDirectory}/.local/bin/switch-keyboard.sh:}%{T1}KB%{T-} ";
        format-suffix = "%{A}";
        format-prefix-foreground = "#${colors.base05.hex.rgb}";
        format-prefix-underline = "#${colors.base0B.hex.rgb}";

        label-layout = "%layout%";
        label-layout-underline = "#${colors.base0B.hex.rgb}";

        label-indicator-padding = 2;
        label-indicator-margin = 1;
        label-indicator-background = "#${colors.base0C.hex.rgb}";
        label-indicator-underline = "#${colors.base0B.hex.rgb}";
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
        format-prefix-foreground = "#${colors.base04.hex.rgb}";
        format-underline = "#${colors.base08.hex.rgb}";
        label = "%percentage:2%%";
      };

      "module/memory" = {
        type = "internal/memory";
        interval = 2;
        format-prefix = "%{A1:${config.programs.urxvt.package}/bin/urxvt -e ${pkgs.htop}/bin/htop --sort-key PERCENT_MEM:}%{T1}MEM%{T-} ";
        format-suffix = "%{A}";
        format-prefix-foreground = "#${colors.base04.hex.rgb}";
        format-underline = "#${colors.base0D.hex.rgb}";
        label = "%percentage_used%%";
      };

      "module/wlan" = {
        type = "internal/network";
        interval = "3.0";

        format-connected = "<ramp-signal> <label-connected>";
        format-connected-underline = "#${colors.base0C.hex.rgb}";
        label-connected = "%essid%";
        format-connected-prefix =
          "%{A1:${config.programs.urxvt.package}/bin/urxvt -e ${pkgs.networkmanager}/bin/nmtui:}%{T1}";
        format-connected-suffix = "%{T-}%{A}";

        format-disconnected = "";

        ramp-signal-0 = "WLAN";
        ramp-signal-1 = "%{F#${colors.base05.hex.rgb}}W%{F#${colors.base04.hex.rgb}}LAN";
        ramp-signal-2 = "%{F#${colors.base05.hex.rgb}}WL%{F#${colors.base04.hex.rgb}}AN";
        ramp-signal-3 = "%{F#${colors.base05.hex.rgb}}WLA%{F#${colors.base04.hex.rgb}}N";
        ramp-signal-4 = "%{F#${colors.base05.hex.rgb}}WLAN%{F#${colors.base04.hex.rgb}}";
        ramp-signal-foreground = "#${colors.base04.hex.rgb}";
      };

      "module/eth" = {
        type = "internal/network";
        interval = "3.0";

        format-connected-underline = "#${colors.base0E.hex.rgb}";
        format-connected-prefix = "ETH ";
        format-connected-prefix-foreground = "#${colors.base04.hex.rgb}";
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

        format-underline = "#${colors.base0D.hex.rgb}";

        label = "%date%%time%";
      };

      "module/alsa" = {
        type = "internal/alsa";

        format-volume = "<label-volume> <bar-volume>";
        label-volume = "%{T1}VOL%{T-}";
        label-volume-foreground = "#${colors.base04.hex.rgb}";

        format-muted-prefix = " ";
        format-muted-foreground = "#${colors.base04.hex.rgb}";
        label-muted = "geluid uit";

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
        full-at = 98;

        format-charging = "%{T1}<animation-charging> <label-charging>%{T-}";
        format-charging-underline = "#${colors.base0B.hex.rgb}";

        format-discharging = "%{T1}<animation-discharging> <label-discharging>%{T-}";
        format-discharging-underline = "#${colors.base0A.hex.rgb}";

        format-full-prefix = "≡ ";
        format-full-prefix-foreground = "#${colors.base04.hex.rgb}";
        format-full-underline = "#${colors.base0B.hex.rgb}";

        ramp-capacity-0 = "";
        ramp-capacity-1 = "";
        ramp-capacity-2 = "";
        ramp-capacity-foreground = "#${colors.base04.hex.rgb}";

        animation-charging-0 = "↑";
        animation-charging-1 = "↑";
        animation-charging-2 = "↑";
        animation-charging-foreground = "#${colors.base04.hex.rgb}";
        animation-charging-framerate = 750;

        animation-discharging-0 = "↓";
        animation-discharging-1 = "↓";
        animation-discharging-2 = "↓";
        animation-discharging-foreground = "#${colors.base04.hex.rgb}";
        animation-discharging-framerate = 750;
      };
    };
  };
}
