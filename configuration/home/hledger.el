(require 'init-keybindings)

(use-package ledger-mode
  :ensure t
  :after general
  :mode
  "\\.leder\\'"
  "\\.journal\\'"
  :custom
  (ledger-binary-path "hledger")
  :general
  (my-local-leader-def
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
    "u" 'ledger-schedule-upcoming))

(provide 'init-ledger)
