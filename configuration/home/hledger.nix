{ pkgs, ... }: {
  home.packages = [
    pkgs.hledger
    pkgs.hledger-web
  ];

  programs.nushell.extraConfig = ''
    source ${pkgs.writers.writeNu "hledger-completions.nu" ./hledger-completions.nu}
    def --wrapped nuhledger [...rest] { hledger -O csv ...$rest | from csv }
  '';

  programs.emacs.init.modules."init/init-ledger.el" = {
    enable = true;
    config = ./hledger.el;
    feature = "init-ledger";
  };
}
