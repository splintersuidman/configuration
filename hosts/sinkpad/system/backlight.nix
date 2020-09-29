# See <https://libreboot.org/docs/misc/#finetune-backlight-control-on-intel-gpus>.
{ pkgs, ... }:
{
  systemd.services.backlight = {
    enable = true;
    description = "Set BLC_PWM_CTL to a good value";
    after = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = "no";
      ExecStart = "${pkgs.intel-gpu-tools}/bin/intel_reg write 0x00061254 0x60016001";
    };
    wantedBy = [ "multi-user.target" "suspend.target" "hibernate.target" "hybrid-sleep.target" ];
  };
}
