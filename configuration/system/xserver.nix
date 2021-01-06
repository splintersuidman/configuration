{ config, pkgs, ... }: {
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

    # NOTE: I configure XMonad from home-manager, but NixOS requires at least
    # one desktop manager or window manager to be enabled. See
    # <https://github.com/nix-community/home-manager/issues/1180#issuecomment-617329614>
    # and for an alternative solution see
    # <https://discourse.nixos.org/t/opening-i3-from-home-manager-automatically/4849/8>.
    desktopManager.xterm.enable = true;

    synaptics = {
      horizEdgeScroll = true;
      vertEdgeScroll = true;
    };
  };
}
