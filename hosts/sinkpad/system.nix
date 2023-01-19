{ pkgs, ... }:
{
  imports = [
    ../../configuration/system.nix
    ./system/hardware-configuration.nix

    ./system/backlight.nix

    ../../configuration/system/gtk.nix
    ../../configuration/system/printing.nix
    ../../configuration/system/shell.nix
    ../../configuration/system/tlp.nix
    ../../configuration/system/tools.nix
    ../../configuration/system/tor.nix
    ../../configuration/system/xserver.nix
  ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  # Use Linux Libre.
  boot.kernelPackages = pkgs.linuxPackages-libre;

  swapDevices = [ { device = "/var/swapfile"; size = 4096; } ];

  networking.hostName = "thinkpad"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "nl_NL.UTF-8";
  };
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Set your time zone.
  time.timeZone = "Europe/Amsterdam";

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [
    5556 # freeciv-server default port
  ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable sound.
  sound.enable = true;
  # hardware.pulseaudio.enable = true;
  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
    # Necessary for mopidy; see
    # <https://github.com/NixOS/nixpkgs/issues/39635#issuecomment-453549679>.
    systemWide = true;
  };

  # OpenGL
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true; # For Steam.

  users.users.splinter = {
    isNormalUser = true;
    extraGroups = [ "wheel" "audio" "networkmanager" "uinput" ];
  };

  nix.settings.trusted-users = [ "root" "@wheel" ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
  system.autoUpgrade.enable = true;
}
