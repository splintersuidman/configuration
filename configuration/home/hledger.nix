{ pkgs, ... }:
{
  home.packages = [
    pkgs.hledger
    pkgs.hledger-web
  ];

  programs.emacs.init.usePackage = {
    ledger-mode = {
      enable = true;
      after = [ "general" ];
      mode = [
        ''("\\.ledger\\'" . ledger-mode)''
        ''("\\.journal\\'" . ledger-mode)''
      ];
      init = ''
        (setq ledger-binary-path "${pkgs.hledger}/bin/hledger")
      '';
      config = ''
        (general-define-key
          :prefix my-local-leader
          :states '(normal visual motion)
          :keymaps 'ledger-mode-map
          "a" 'ledger-add-transaction
          "b" 'ledger-post-edit-amount
          "c" 'ledger-toggle-current
          "d" 'ledger-delete-current-transaction
          "e" 'ledger-toggle-current-transaction
          "f" 'ledger-occur
          "k" 'ledger-copy-transaction-at-point
          "l" 'ledger-display-ledger-stats
          "oa" 'ledger-report-redo
          "oe" 'ledger-report-edit-report
          "og" 'ledger-report-goto
          "ok" 'ledger-report-quit
          "or" 'ledger-report
          "os" 'ledger-report-save
          "p" 'ledger-display-balance-at-point
          "q" 'ledger-post-align-xact
          "r" 'ledger-reconcile
          "s" 'ledger-sort-region
          "t" 'ledger-insert-effective-date
          "u" 'ledger-schedule-upcoming)
      '';
    };
  };
}
