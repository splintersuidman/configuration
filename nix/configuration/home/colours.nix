{ ... }:
let
  gruvboxDark = rec {
    background = black;
    foreground = brightWhite;
    black = "#282828";
    brightBlack = "#928374";
    red = "#cc241d";
    brightRed = "#fb4934";
    green = "#98971a";
    brightGreen = "#b8bb26";
    yellow = "#d79921";
    brightYellow = "#fabd2f";
    blue = "#458588";
    brightBlue = "#83a598";
    magenta = "#b16286";
    brightMagenta = "#d3869b";
    cyan = "#689d6a";
    brightCyan = "#8ec07c";
    white = "#a89984";
    brightWhite = "#ebdbb2";

    background0 = "#282828";
    background1 = "#3c3836";
    background2 = "#504945";
    background3 = "#665c54";
    background4 = "#7c6f64";
    foreground0 = "#fbf1c7";
    foreground1 = "#ebdbb2";
    foreground2 = "#d5c4a1";
    foreground3 = "#bdae93";
    foreground4 = "#a89984";
  };

  dracula = rec {
    background = "#282a36";
    foreground = "#f8f8f2";

    black = "#44475a";
    brightBlack = "#44475a";
    red = "#ff5555";
    brightRed = "#ff5555";
    green = "#50fa7b";
    brightGreen = "#50fa7b";
    yellow = "#ffb86c"; # orange
    brightYellow = "#f1fa8c";
    blue = "#bd93f9";
    brightBlue = "#bd93f9";
    magenta = "#ff79c6";
    brightMagenta = "#ff79c6";
    cyan = "#8be9fd";
    brightCyan = "#8be9fd";
    white = "#f8f8f2";
    brightWhite = "#f8f8f2";

    background0 = "#282a36";
    background1 = "#282a36";
    background2 = "#44475a";
    background3 = "#44475a";
    background4 = "#6272a4";
    foreground0 = "#f8f8f2";
    foreground1 = "#f8f8f2";
    foreground2 = "#f8f8f2";
    foreground3 = "#f8f8f2";
    foreground4 = "#f8f8f2";
  };

  spacemacsThemeDark = rec {
    black = background0;
    brightBlack = background2;
    red = "#f2241f";
    brightRed = red;
    green = "#67b11d";
    brightGreen = green;
    yellow = "#b1951d";
    brightYellow = yellow;
    blue = "#4f97d7";
    brightBlue = "#2d9574";
    magenta = "#a31db1";
    brightMagenta = magenta;
    cyan = "#28def0";
    brightCyan = cyan;
    white = "#a89984";
    brightWhite = white;

    background = background0;
    background0 = background1;
    background1 = "#292b2e";
    background2 = "#212026";
    background3 = "#100a14";
    background4 = "#0a0814";
  };

  greenDark = rec {
    black = "#373a42";
    brightBlack = black;
    red = "#ec6363";
    brightRed = red;
    green = "#4adb5c";
    brightGreen = green;
    yellow = "#a6c35e";
    brightYellow = yellow;
    blue = "#4697db";
    brightBlue = blue;
    magenta = "#b883d8";
    brightMagenta = magenta;
    cyan = "#4eb6a9";
    brightCyan = cyan;
    white = "#a5a5a5";
    brightWhite = white;

    foreground = foreground0;
    foreground0 = "#787878";
    foreground1 = foreground0;
    foreground2 = white;
    foreground3 = white;
    foreground4 = "#dddddd";

    background = background0;
    background0 = "#000000";
    background1 = "#111111";
    background2 = "#222222";
    background3 = "#333333";
    background4 = "#444444";

    colour0 = background;
  };

  greenLight = rec {
    black = "#373a42";
    brightBlack = black;
    red = "#ec6363";
    brightRed = red;
    green = "#4adb5c";
    brightGreen = green;
    yellow = "#a6c35e";
    brightYellow = yellow;
    blue = "#4697db";
    brightBlue = blue;
    magenta = "#b883d8";
    brightMagenta = magenta;
    cyan = "#4eb6a9";
    brightCyan = cyan;
    white = "#a5a5a5";
    brightWhite = white;

    foreground = foreground0;
    foreground0 = "#000000";
    foreground1 = foreground0;
    foreground2 = "#111111";
    foreground3 = foreground2;
    foreground4 = "#4b4b4b";

    background = background0;
    background0 = "#eeeeee";
    background1 = "#dddddd";
    background2 = "#787878";
    background3 = background2;
    background4 = background3;

    colour0 = background;
  };

  base16TomorrowNight = rec {
    black = "#1d1f21";
    brightBlack = "#969896";
    red = "#cc6666";
    brightRed = red;
    green = "#b5bd68";
    brightGreen = green;
    yellow = "#f0c674";
    brightYellow = yellow;
    blue = "#81a2be";
    brightBlue = blue;
    magenta = "#b294bb";
    brightMagenta = magenta;
    cyan = "#8abeb7";
    brightCyan = cyan;
    white = "#c5c8c6";
    brightWhite = "#ffffff";

    background = black;
    foreground = white;
  };
in
{
  imports = [
    ../../modules/home/colours.nix
  ];

  colours = base16TomorrowNight;
}
