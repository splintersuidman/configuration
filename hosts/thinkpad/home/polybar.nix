{ ... }: {
  services.polybar.config = {
    "module/battery" = {
      battery = "BAT0";
      adapter = "AC";
    };
    "module/wlan".interface = "wlp3s0";
    "module/eth".interface = "enp2s0f0";
  };
}
