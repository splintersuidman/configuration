{ ... }: {
  services.logind = let action = "suspend-then-hibernate";
  in {
    lidSwitch = action;
    lidSwitchDocked = action;
    extraConfig = ''
      HandlePowerKey=${action}
    '';
  };
  systemd.sleep.extraConfig = ''
    HibernateDelaySec=1h
  '';
}
