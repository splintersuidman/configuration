(require 'init-keybindings)

(use-package tab-bar
  :config
  (tab-bar-mode t)
  :custom
  (tab-bar-show 1)
  :general
  (my-leader-def
    "t RET" '(tab-bar-switch-to-tab :which-key "Switch tab")
    "tF" '(toggle-frame-tab-bar :which-key "Toggle frame tab bar")
    "tN" '(tab-bar-new-tab :which-key "New tab")
    "tb" '(switch-to-buffer-other-tab :which-key "Switch to buffer other tab")
    "td" '(tab-bar-close-tab :which-key "Close tab")
    "tf" '(find-file-other-tab :which-key "Find file other tab")
    "tn" '(tab-bar-switch-to-next-tab :which-key "Next tab")
    "to" '(other-tab-prefix :which-key "Other tab prefix")
    "tp" '(tab-bar-switch-to-prev-tab :which-key "Previous tab")
    "tr" '(tab-bar-rename-tab :which-key "Renam tabe")
    "tt" '(tab-bar-switch-to-tab :which-key "Switch tab")
    "tu" '(tab-bar-undo-close-tab :which-key "Undo close tab")))

(provide 'init-tab-bar)
