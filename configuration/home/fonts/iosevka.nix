{ pkgs, ... }:
let
  uprightLetters =
    [ "cv24" "cv69" "cv72" "cv74" "cv76" "cv78" "cv79" "cv81" "cv83" "cv95" ];
  italicLetters = [
    "VXBU"
    "cv24"
    "VXCC"
    "VXBR"
    "VXCD"
    "VXCG"
    "VXBA"
    "cv72"
    "cv76"
    "cv78"
    "cv79"
    "cv74"
    "cv81"
    "cv83"
    "cv95"
  ];
  numbers = [ "VXAT" ];
  symbols = [ "cv19" "VXAH" "cv33" "cv45" "cv67" ];

  iosevkaCustom = pkgs.iosevka.override {
    set = "custom";
    privateBuildPlan = {
      family = "Iosevka Custom";
      design = [ "sans" "ligset-haskell" ] ++ numbers ++ symbols;
      upright = uprightLetters;
      oblique = uprightLetters;
      italic = italicLetters;
    };
  };
  iosevkaAile = pkgs.iosevka.override { set = "aile"; };
in { home.packages = [ iosevkaCustom iosevkaAile ]; }
