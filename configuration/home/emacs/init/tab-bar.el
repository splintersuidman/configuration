;;; -*- lexical-binding: t -*-

(require 'init-keybindings)

(use-package tab-bar
  :config
  (tab-bar-mode t)
  (defun splinter-tab-bar-move-tab-left (&optional arg)
    "Move the current tab ARG positions to the left."
    (interactive "p")
    (tab-bar-move-tab (- arg)))
  (defun splinter-tab-bar-move-tab-right (&optional arg)
    "Move the current tab ARG positions to the right."
    (interactive "p")
    (tab-bar-move-tab arg))
  (defun splinter-tab-bar-switch-to-last-tab ()
    "Switch to the last selected tab."
    (interactive)
    (when-let* ((last-tab (car (tab-bar--tabs-recent))))
      (tab-bar-select-tab (1+ (tab-bar--tab-index last-tab)))))
  :custom
  (tab-bar-show 1)
  :general
  (my-leader-def
    "t'" '(splinter-tab-bar-switch-to-last-tab :which-key "Last tab")
    "t RET" '(tab-bar-switch-to-tab :which-key "Switch tab")
    "tF" '(toggle-frame-tab-bar :which-key "Toggle frame tab bar")
    "tN" '(tab-bar-new-tab :which-key "New tab")
    "tH" '(splinter-tab-bar-move-tab-left :which-key "Move left")
    "tL" '(splinter-tab-bar-move-tab-right :which-key "Move right")
    "tb" '(switch-to-buffer-other-tab :which-key "Switch to buffer other tab")
    "td" '(tab-bar-close-tab :which-key "Close tab")
    "tf" '(find-file-other-tab :which-key "Find file other tab")
    "tn" '(tab-bar-switch-to-next-tab :which-key "Next tab")
    "to" '(other-tab-prefix :which-key "Other tab prefix")
    "tp" '(tab-bar-switch-to-prev-tab :which-key "Previous tab")
    "tr" '(tab-bar-rename-tab :which-key "Rename tab")
    "tt" '(tab-bar-switch-to-tab :which-key "Switch tab")
    "tu" '(tab-bar-undo-close-tab :which-key "Undo close tab")))

(provide 'init-tab-bar)
