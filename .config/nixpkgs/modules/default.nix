{ pkgs, config, ... }:
let
  warnMissing = with builtins; files:
    concatMap
      (filePath:
        # NOTE: must use fileString, because evaluating filePath will
        # result in an abortion when the file does not exist.
        let fileString = toString filePath; in
        if pathExists fileString then
          [filePath]
        else
          trace
            "${./.}: when loading modules:\nFile ${fileString} does not exist."
            []
      )
      files;
in
{
  imports = warnMissing [
    ./bash.nix
    ./colours.nix
    ./compton.nix
    ./dunst.nix
    ./emacs.nix
    ./email.nix
    ./firefox.nix
    ./git.nix
    ./gpg.nix
    ./keyboard.nix
    ./lorri.nix
    ./mopidy.nix
    ./mpdscribble.nix
    ./ncmpcpp.nix
    ./newsboat.nix
    ./onedrive.nix
    ./pass.nix
    ./polybar.nix
    ./redshift.nix
    ./texlive.nix
    ./tmux.nix
    ./unclutter.nix
    ./urxvt.nix
    ./xdg.nix
    ./xresources.nix
    ./xsession.nix
    ./zathura.nix
    
    ./misc
    ./programs
    ./services
  ];
}
