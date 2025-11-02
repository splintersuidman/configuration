;;; -*- lexical-binding: t -*-

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

(use-package reformatter
  :ensure t)

(use-package hledger-mode
  :ensure t
  :after (ledger-mode general reformatter)
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
  :config
  (reformatter-define hledger-format
    :program "hledger-fmt"
    :args '("--no-diff" "--exit-zero-on-changes" "-")
    :group 'hledger-mode
    :lighter " Hledger-Fmt")
  ;;;###autoload (autoload 'hledger-format-buffer "hledger-mode" nil t)
  ;;;###autoload (autoload 'hledger-format-region "hledger-mode" nil t)
  ;;;###autoload (autoload 'hledger-format-on-save-mode "hledger-mode" nil t)
  :hook
  (hledger-mode . splinter-hledger-setup-completion)
  :general
  (my-local-leader-def
    :keymaps 'hledger-mode-map
    "TAB" '(hledger-append-clipboard-to-journal :which-key "Append clipboard to journal")
    "b" '(hledger-edit-amount :which-key "Edit amount")
    "d" '(hledger-reschedule :which-key "Reschedule")
    "f" '(hledger-format-buffer :which-key "Format buffer")
    "F" '(hledger-format-region :which-key "Format region")
    "n" '(hledger-next-or-new-entry :which-key "Next or new entry")
    "p" '(hledger-backward-entry :which-key "Previous entry")
    "r" '(hledger-run-command :which-key "Run command")))

(provide 'init-ledger)
