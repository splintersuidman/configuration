{ ... }: {
  imports = [
    ../../configuration/home.nix

    ./home/polybar.nix

    ../../configuration/home/agda.nix
    ../../configuration/home/audio.nix
    ../../configuration/home/autorandr.nix
    ../../configuration/home/browser.nix
    ../../configuration/home/c.nix
    ../../configuration/home/chat.nix
    ../../configuration/home/coq.nix
    ../../configuration/home/direnv.nix
    ../../configuration/home/dunst.nix
    ../../configuration/home/elfeed.nix
    ../../configuration/home/emacs.nix
    ../../configuration/home/feh.nix
    ../../configuration/home/fonts.nix
    ../../configuration/home/games.nix
    ../../configuration/home/git.nix
    ../../configuration/home/gpg.nix
    ../../configuration/home/gtk.nix
    ../../configuration/home/haskell.nix
    ../../configuration/home/hledger.nix
    ../../configuration/home/home-manager.nix
    ../../configuration/home/htop.nix
    ../../configuration/home/keyboard.nix
    ../../configuration/home/language.nix
    ../../configuration/home/lean.nix
    ../../configuration/home/lorri.nix
    ../../configuration/home/mail.nix
    ../../configuration/home/markdown.nix
    ../../configuration/home/misc.nix
    ../../configuration/home/mopidy.nix
    ../../configuration/home/mpdscribble.nix
    ../../configuration/home/mpv.nix
    ../../configuration/home/ncmpcpp.nix
    ../../configuration/home/nix.nix
    ../../configuration/home/ocaml.nix
    ../../configuration/home/onedrive.nix
    ../../configuration/home/org-mode.nix
    ../../configuration/home/password.nix
    ../../configuration/home/pdf.nix
    ../../configuration/home/polybar.nix
    ../../configuration/home/purescript.nix
    ../../configuration/home/python.nix
    ../../configuration/home/r.nix
    ../../configuration/home/redshift.nix
    ../../configuration/home/rust.nix
    ../../configuration/home/sage.nix
    ../../configuration/home/scheme.nix
    ../../configuration/home/shell.nix
    ../../configuration/home/ssh.nix
    ../../configuration/home/tex.nix
    ../../configuration/home/theme.nix
    ../../configuration/home/tmux.nix
    ../../configuration/home/tools.nix
    ../../configuration/home/typst.nix
    ../../configuration/home/unclutter.nix
    ../../configuration/home/urxvt.nix
    ../../configuration/home/vterm.nix
    ../../configuration/home/xdg.nix
    ../../configuration/home/xresources.nix
    ../../configuration/home/xsession.nix
    ../../configuration/home/yaml.nix
    ../../configuration/home/zig.nix
  ];

  home = {
    username = "splinter";
    homeDirectory = "/home/splinter";
    stateVersion = "20.09";
  };
}
