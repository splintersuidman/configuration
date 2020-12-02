{ pkgs, config, ... }:
{
  programs.bash = {
    enable = true;
    historyFile = "${config.xdg.dataHome}/bash/bash_history";
    shellAliases = {
      ".." = "cd ..";
      "..." = "cd ../..";
      "ls" = "${pkgs.exa}/bin/exa";
    };
    shellOptions = [ "autocd" ];
    initExtra = ''
      # Enable vi mode
      set -o vi
    '';
  };

  programs.readline = {
    enable = true;
    extraConfig = ''
      set completion-ignore-case on
    '';
  };

  programs.autojump.enable = true;
}
