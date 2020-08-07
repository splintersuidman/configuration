{ pkgs, config, ... }:
{
  home.keyboard = {
    layout = "us";
    options = [
      "ctrl:nocaps"
      "compose:menu"
      "compose:ralt"
    ];
  };

  services.xcape = {
    enable = true;
    mapExpression = {
      Control_L = "Escape";
    };
  };
}
