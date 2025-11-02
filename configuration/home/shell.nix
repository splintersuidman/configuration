{ pkgs, config, ... }: {
  programs.bash = {
    enable = true;
    historyFile = "${config.xdg.dataHome}/bash/bash_history";
    shellAliases = {
      ".." = "cd ..";
      "..." = "cd ../..";
      "ls" = "${pkgs.eza}/bin/eza";
    };
    shellOptions = [ "autocd" ];
    initExtra = ''
      # Enable vi mode if not running vterm inside Emacs.
      if [[ "$INSIDE_EMACS" != 'vterm' ]]; then
        set -o vi
      fi
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
