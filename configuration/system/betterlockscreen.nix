{ pkgs, ... }:
{
  systemd.services.betterlockscreen = {
    enable = true;
    description = "Locks screen";
    environment = { DISPLAY = ":0.0"; };
    serviceConfig = {
      User = "splinter";
      ExecStart = ''${pkgs.betterlockscreen}/bin/betterlockscreen -l blur -t "Typ wachtwoord..."'';
      TimeoutSec = "infinity";
    };
    wantedBy = [ "sleep.target" "suspend.target" ];
  };
}
