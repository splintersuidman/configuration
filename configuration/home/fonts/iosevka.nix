{ pkgs, ... }:
let
  iosevkaCustom = pkgs.iosevka.override {
    set = "custom";
    privateBuildPlan = {
      family = "Iosevka Custom";
      spacing = "normal";
      serifs = "sans";
      noCvSs = false;
      exportGlyphNames = false;

      variants.inherits = "ss05";
      ligations.inherits = "haskell";

      weights = {
        Regular = {
          shape = 400;
          menu = 400;
          css = 400;
        };
        Medium = {
          shape = 500;
          menu = 500;
          css = 500;
        };
        Bold = {
          shape = 700;
          menu = 700;
          css = 700;
        };
      };

      slopes = {
        Upright = {
          angle = 0;
          shape = "upright";
          menu = "upright";
          css = "normal";
        };

        Italic = {
          angle = 9.4;
          shape = "italic";
          menu = "italic";
          css = "italic";
        };
      };
    };
  };
in { home.packages = [ iosevkaCustom ]; }
