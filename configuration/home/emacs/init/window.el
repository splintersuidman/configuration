;;; -*- lexical-binding: t -*-

(require 'init-keybindings)

(use-package window
  :custom
  (window-combination-resize t)
  (display-buffer-alist 
   '(("\\*\\(Help\\|WoMan\\).*"
      (display-buffer-in-side-window)
      (window-width . 0.30)
      (side . right)
      (slot . 0))
     ("\\*eldoc.*"
      (display-buffer-in-side-window)
      (window-width . 0.30)
      (side . right)
      (slot . 1))
     ("\\*.*\\(e?shell\\|v?term\\).*\\*"
      (display-buffer-reuse-mode-window display-buffer-at-bottom)
      (window-height . 0.30))
     ("\\*Flymake diagnostics for.*\\*"
      (display-buffer-reuse-mode-window display-buffer-at-bottom)
      (window-height . 0.30))
     ("*evil-owl*"
      (display-buffer-in-side-window)
      (side . bottom)
      (window-height . 0.3))))
  :general
  (my-leader-def
    "w'" '(evil-window-mru :which-key "Previous window")
    "w0" '(delete-other-windows :which-key "Delete other windows")
    "w=" '(balance-windows :which-key "Balance")
    "wH" '(windmove-swap-states-left :which-key "Swap left")
    "wJ" '(windmove-swap-states-down :which-key "Swap down")
    "wK" '(windmove-swap-states-up :which-key "Swap up")
    "wL" '(windmove-swap-states-right :which-key "Swap right")
    "wd" '(delete-window :which-key "Delete")
    "wh" '(evil-window-left :which-key "Left")
    "wj" '(evil-window-down :which-key "Down")
    "wk" '(evil-window-up :which-key "Up")
    "wl" '(evil-window-right :which-key "Right")
    "wo" '(other-window :which-key "Other window")
    "ws" '(split-window-vertically :which-key "Split vertically")
    "wv" '(split-window-horizontally :which-key "Split horizontally")
    "ww" '(evil-window-mru :which-key "Previous window")))

(use-package winner
  :disabled
  :config
  (winner-mode)
  :general
  (my-leader-def
    "wr" '(winner-redo :which-key "Redo")
    "wu" '(winner-undo :which-key "Undo")))

;; `winner-mode' works across tabs, which I do not like, so I use
;; `tab-bar-history-mode' instead.
(use-package tab-bar
  :config
  (tab-bar-history-mode)
  :general
  (my-leader-def
    "wr" '(tab-bar-history-forward :which-key "Redo")
    "wu" '(tab-bar-history-back :which-key "Undo")))

(provide 'init-window)
