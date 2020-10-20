{ ... }: {
  services.polybar.config = {
    "module/battery" = {
      battery = "BAT0";
      adapter = "AC";
    };
    "module/wlan".interface = "wlp2s0";
    "module/eth".interface = "enp0s25";
  };
}
