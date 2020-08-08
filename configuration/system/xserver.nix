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
    };

    synaptics = {
      horizEdgeScroll = true;
      vertEdgeScroll = true;
    };
  };
}
