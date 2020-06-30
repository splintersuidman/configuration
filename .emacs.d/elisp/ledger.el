(use-package ledger-mode
  :mode ("\\.ledger\\'" "\\.journal\\'")
  :init
  (setq ledger-binary-path "hledger")
  :config
  (evil-leader/set-key-for-mode 'ledger-mode
    "ca" 'ledger-add-transaction
    "cb" 'ledger-post-edit-amount
    "cc" 'ledger-toggle-current
    "cd" 'ledger-delete-current-transaction
    "ce" 'ledger-toggle-current-transaction
    "cf" 'ledger-occur
    "ck" 'ledger-copy-transaction-at-point
    "cl" 'ledger-display-ledger-stats
    "coa" 'ledger-report-redo
    "coe" 'ledger-report-edit-report
    "cog" 'ledger-report-goto
    "cok" 'ledger-report-quit
    "cor" 'ledger-report
    "cos" 'ledger-report-save
    "cp" 'ledger-display-balance-at-point
    "cq" 'ledger-post-align-xact
    "cr" 'ledger-reconcile
    "cs" 'ledger-sort-region
    "ct" 'ledger-insert-effective-date
    "cu" 'ledger-schedule-upcoming))
