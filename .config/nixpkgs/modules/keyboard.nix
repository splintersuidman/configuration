{ pkgs, config, ... }:
{
  home.keyboard = {
    layout = "us";
    options = [
      "ctrl:nocaps"
      "compose:menu"
    ];
  };

  services.xcape = {
    enable = true;
    mapExpression = {
      Control_L = "Escape";
    };
  };

  xsession.initExtra =
    let xset = "${pkgs.xorg.xset}/bin/xset"; in ''
      ${xset} r rate 200 25
    '';
}
