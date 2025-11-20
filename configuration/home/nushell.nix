{ pkgs, config, ... }:
let script = path: "${pkgs.nu_scripts}/share/nu_scripts/${path}";
in {
  home.packages = [ pkgs.nufmt ];

  programs.nushell = {
    enable = true;
    environmentVariables.PROMPT_COMMAND_RIGHT = "";
    settings.show_banner = false;
    extraConfig = ''
      source ${script "custom-completions/bitwarden-cli/bitwarden-cli-completions.nu"}
      source ${script "custom-completions/cargo/cargo-completions.nu"}
      source ${script "custom-completions/git/git-completions.nu"}
      source ${script "custom-completions/just/just-completions.nu"}
      source ${script "custom-completions/less/less-completions.nu"}
      source ${script "custom-completions/nix/nix-completions.nu"}
      source ${script "custom-completions/rg/rg-completions.nu"}
      source ${script "custom-completions/ssh/ssh-completions.nu"}
      source ${script "custom-completions/tar/tar-completions.nu"}
      source ${script "custom-completions/typst/typst-completions.nu"}

      use ${script "modules/nix/nix.nu"} ns
    '';
  };

  programs.emacs.init.modules."init/init-nushell.el" = {
    enable = true;
    config = ./nushell.el;
    feature = "init-nushell";
  };
}
