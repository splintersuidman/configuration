{ pkgs, ... }:
{
  home.packages = [
    pkgs.hledger
    pkgs.hledger-web
  ];

  programs.emacs.init.modules."init/init-ledger.el" = {
    enable = true;
    config = ./hledger.el;
    feature = "init-ledger";
  };
}
