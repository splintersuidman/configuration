{ config, pkgs, ... }:
{
  services.xserver = {
    enable = true;
    libinput.enable = true;
    autoRepeatDelay = 200;
    autoRepeatInterval = 25;

    displayManager = {
      lightdm = {
        enable = true;
        greeters.gtk = {
          enable = true;
          cursorTheme = {
            package = pkgs.gnome3.defaultIconTheme;
            name = "Adwaita";
            size = 18;
          };
        };
      };

      sessionCommands = ''
        ${pkgs.lightlocker}/bin/light-locker --lock-on-lid --lock-on-suspend --lock-after-screensaver=0 &
      '';
    };
    windowManager.xmonad.enable = true;

    synaptics = {
      horizEdgeScroll = true;
      vertEdgeScroll = true;
    };
  };
}
