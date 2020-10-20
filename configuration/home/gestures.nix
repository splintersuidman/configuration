{ pkgs, ... }:
let
  xdotool = "${pkgs.xdotool}/bin/xdotool";
  gesturesConfig = pkgs.writeText "libinput-gestures.conf" ''
    # Workspace left and right
    gesture swipe left     3   ${xdotool} key super+h
    gesture swipe right    3   ${xdotool} key super+l

    # Scratchpad
    gesture pinch out      4   ${xdotool} key super+s
    gesture pinch in       4   ${xdotool} key super+s
  '';
in
{
  systemd.user.services.libinput-gestures = {
    Unit = {
      Description = "Touchpad gestures";
    };
    Service = {
      ExecStart = "${pkgs.libinput-gestures}/bin/libinput-gestures -c ${gesturesConfig}";
    };
  };
}
