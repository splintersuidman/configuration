{ ... }: {
  services.logind = rec {
    lidSwitch = "suspend-then-hibernate";
    extraConfig = ''
      HandlePowerKey=${lidSwitch}
    '';
  };
  systemd.sleep.extraConfig = ''
    HibernateDelaySec=30m
  '';
}
