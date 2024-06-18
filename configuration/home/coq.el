(require 'init-keybindings)

(use-package proof-general
  :ensure t
  :mode ("\\.v\\'" . coq-mode)
  :general
  (my-local-leader-def
    :keymaps 'coq-mode-map
    "b" '(proof-process-buffer :which-key "Process buffer")
    "c" '(proof-interrupt-process :which-key "Interrupt process")
    "d" '(proof-tree-external-display-toggle :which-key "External display toggle")
    "f" '(proof-find-theorems :which-key "Find theorems")
    "TAB" '(proof-query-identifier :which-key "Query identifier")
    "l" '(proof-layout-windows :which-key "Layout windows")
    "RET" '(proof-goto-point :which-key "Goto point")
    "n" '(proof-assert-next-command-interactive :which-key "Assert next command")
    "o" '(proof-display-some-buffers :which-key "Display some buffers")
    "p" '(proof-prf :which-key "Proof state")
    "r" '(proof-retract-buffer :which-key "Retract buffer")
    "s" '(proof-toggle-active-scripting :which-key "Toggle active scripting")
    "t" '(proof-ctxt :which-key "Context")
    "u" '(proof-undo-last-successful-command :which-key "Undo last successful command")
    "v" '(proof-minibuffer-cmd :which-key "Minibuffer command")
    "w" '(pg-response-clear-displays :which-key "Response clear displays")
    "x" '(proof-shell-exit :which-key "Shell exit")
    "z" '(proof-frob-locked-end :which-key "Frob locked end")
    ">" '(proof-autosend-toggle :which-key "Autosend toggle")
    "`" '(proof-next-error :which-key "Next error")
    "." '(proof-goto-end-of-locked :which-key "Goto end of locked")
    ";" '(pg-insert-last-output-as-comment :which-key "Insert last output as comment")
    "<backspace>" '(proof-undo-and-delete-last-successful-command :which-key "Undo and delete last successful command")))

(provide 'init-coq)
