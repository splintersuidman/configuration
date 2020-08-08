{ pkgs, config, ... }:
{
  programs.bash = {
    enable = true;
    historyFile = "${config.xdg.dataHome}/bash/bash_history";
    shellAliases = {
      ".." = "cd ..";
      "..." = "cd ../..";
    };
    enableAutojump = true;
    shellOptions = [ "autocd" ];
    profileExtra = ''
      # Enable vi mode
      set -o vi
    '';
  };
}
