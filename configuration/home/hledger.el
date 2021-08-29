(require 'init-keybindings)

(use-package ledger-mode
  :disabled
  :ensure t
  :after general
  :mode
  "\\.ledger\\'"
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

(use-package hledger-mode
  :ensure t
  :mode
  "\\.ledger\\'"
  "\\.journal\\'"
  :custom
  (hledger-jfile "~/.hledger.journal")
  (hledger-comments-column 4)
  (hledger-currency-string "€")
  :init
  (defun splinter-hledger-setup-completion ()
    (make-local-variable 'completion-at-point-functions)
    (add-to-list 'completion-at-point-functions 'hledger-completion-at-point))
  :hook
  (hledger-mode . splinter-hledger-setup-completion)
  :general
  (my-local-leader-def
    :keymaps 'hledger-mode-map
    "TAB" '(hledger-append-clipboard-to-journal :which-key "Append clipboard to journal")
    "b" '(hledger-edit-amount :which-key "Edit amount")
    "d" '(hledger-reschedule :which-key "Reschedule")
    "n" '(hledger-next-or-new-entry :which-key "Next or new entry")
    "p" '(hledger-backward-entry :which-key "Previous entry")
    "r" '(hledger-run-command :which-key "Run command")))

(provide 'init-ledger)
