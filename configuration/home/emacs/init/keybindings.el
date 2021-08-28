(use-package evil
  :ensure t
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil)
  (evil-echo-state nil)
  (evil-want-minibuffer t)
  :config
  (evil-mode))

(use-package evil-collection
  :ensure t
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package undo-tree
  :ensure t
  :after evil
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode))

(use-package evil-numbers
  :ensure t
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(defconst my-leader "SPC"
  "Leader key, used for top-level commands.")

(defconst my-local-leader (concat my-leader " c")
  "Local leader key, used for mode-specific commands.")

(use-package general
  :ensure t
  :after evil
  :custom
  (general-override-states '(insert emacs hybrid normal visual motion operator replace))
  :config
  (general-create-definer my-leader-def
    :states '(normal visual motion)
    :prefix my-leader
    :keymaps 'override)
  (general-create-definer my-local-leader-def
    :states '(normal visual motion)
    :prefix my-local-leader)

  (my-leader-def
    ;; Unbind leader key
    "" nil

    ;; Prefix keys
    "b" '(:ignore t :which-key "Buffer")
    "c" '(:ignore t :which-key "Local")
    "e" '(:ignore t :which-key "Editing")
    "f" '(:ignore t :which-key "File")
    "g" '(:ignore t :which-key "Git")
    "h" '(:ignore t :which-key "Help")
    "j" '(:ignore t :which-key "Jump")
    "l" '(:ignore t :which-key "Lsp")
    "o" '(:ignore t :which-key "Org-mode")
    "p" '(:ignore t :which-key "Project")
    "s" '(:ignore t :which-key "Scratch")
    "t" '(:ignore t :which-key "Theme")
    "w" '(:ignore t :which-key "Window")
    "z" '(:ignore t :which-key "Org-roam")

    ;; Buffer-related commands under `b'
    "bb" '(consult-buffer :which-key "Switch buffer")
    "bB" '(list-buffers :which-key "List buffers")
    "bd" '((lambda ()
             (interactive)
             (when (y-or-n-p "Kill this buffer? ")
               (kill-this-buffer)))
           :which-key "Kill this buffer")
    "bD" '(kill-this-buffer :which-key "Force kill this buffer")
    "bk" '(kill-buffer :which-key "Kill buffer")
    "bo" '(consult-buffer-other-window :which-key "Switch buffer in other window")

    ;; Editing-related commands under `e'
    "es" '(evil-ex-sort :which-key "Sort")

    ;; File-related commands under `f'
    "fd" '(dired :which-key "Dired")
    "ff" '(find-file :which-key "Find file")
    "fF" '(consult-find :which-key "Find file")
    "fp" '(find-file-at-point :which-key "Find file at point")
    "fr" '(consult-ripgrep :which-key "Ripgrep")
    "fs" '(save-buffer :which-key "Save buffer")

    ;; Window-related commands under `w'
    "wh" '(evil-window-left :which-key "Left")
    "wj" '(evil-window-down :which-key "Down")
    "wk" '(evil-window-up :which-key "Up")
    "wl" '(evil-window-right :which-key "Right")
    "wH" '(windmove-swap-states-left :which-key "Swap left")
    "wJ" '(windmove-swap-states-down :which-key "Swap down")
    "wK" '(windmove-swap-states-up :which-key "Swap up")
    "wL" '(windmove-swap-states-right :which-key "Swap right")
    "wo" '(other-window :which-key "Other window")
    "wv" '(split-window-horizontally :which-key "Split horizontally")
    "ws" '(split-window-vertically :which-key "Split vertically")
    "wd" '(delete-window :which-key "Delete")
    "w=" '(balance-windows :which-key "Balance")

    "SPC" '(execute-extended-command :which-key "M-x")

    ;; Editing-related commands
    ";" '(comment-dwim :which-key "Comment")
    "+" '(evil-numbers/inc-at-pt :which-key "Increase at point")
    "-" '(evil-numbers/dec-at-pt :which-key "Decrease at point")))

(provide 'init-keybindings)