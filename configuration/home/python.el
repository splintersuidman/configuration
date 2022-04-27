(require 'init-keybindings)

(use-package python
  :general
  (my-local-leader-def
    :keymaps 'python-mode-map
    "c" '(python-shell-send-buffer :which-key "Send buffer")
    "e" '(python-shell-send-statement :which-key "Send statement")
    "l" '(python-shell-send-file :which-key "Send file")
    "p" '(run-python :which-key "Run Python")
    "r" '(python-shell-send-region :which-key "Send region")
    "s" '(python-shell-send-region :which-key "Send string")
    "tc" '(python-skeleton-class :which-key "Class")
    "td" '(python-skeleton-def :which-key "Function")
    "tf" '(python-skeleton-for :which-key "For")
    "ti" '(python-skeleton-if :which-key "If")
    "tm" '(python-skeleton-import :which-key "Import")
    "tt" '(python-skeleton-try :which-key "Try")
    "tw" '(python-skeleton-while :which-key "While")
    "z" '(python-shell-switch-to-shell :which-key "Switch to shell")))

(use-package python-black
  :ensure t
  :after python
  :config
  (defun splinter-python-black-partial-dwim (&optional display-errors)
    "Reformat the active region or the current buffer.

This runs ‘python-black-region’ or ‘python-black-buffer’
depending on whether the region is currently active.

When called interactively with a prefix argument, or when
DISPLAY-ERRORS is non-nil, shows a buffer if the formatting
fails."
    (interactive "p")
    (if (region-active-p)
        (python-black-region (region-beginning) (region-end) display-errors)
      (python-black-buffer display-errors)))
  :general
  (my-local-leader-def
    :keymaps 'python-mode-map
    "f" '(splinter-python-black-partial-dwim :which-key "Format")))

;; TODO: isortify

(provide 'init-python)
