{ pkgs, config, ... }:
{
  programs.bash = {
    enable = true;
    shellAliases = {
      ".." = "cd ..";
      "..." = "cd ../..";
    };
  };
}
