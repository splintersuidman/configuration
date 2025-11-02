{ pkgs, ... }: {
  home.packages = [
    pkgs.unstable.hledger
    pkgs.unstable.hledger-web
    pkgs.unstable.hledger-fmt
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
