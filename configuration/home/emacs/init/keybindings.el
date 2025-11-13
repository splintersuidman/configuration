;;; -*- lexical-binding: t -*-

(use-package evil
  :ensure t
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil)
  (evil-echo-state nil)
  (evil-want-minibuffer t)
  :config
  (defun splinter-evil-quit (oldfun &rest r)
    "Advice for `evil-quit' to kill tabs before frames.

The order of closing then is: current window, current tab,
current frame, Emacs."
    (if (and (eq (length (window-list)) 1)
             (> (length (tab-bar-tabs)) 1))
        (tab-bar-close-tab)
      (apply oldfun r)))
  (advice-add 'evil-quit :around 'splinter-evil-quit)

  (evil-mode))

(use-package evil-collection
  :ensure t
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-goggles
  :ensure t
  :custom
  (evil-goggles-duration 0.15)
  :config
  (evil-goggles-mode))

(use-package evil-owl
  :ensure t
  :after window
  :custom
  (evil-owl-display-method 'window)
  :config
  (evil-owl-mode))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode))

(use-package undo-fu
  :ensure t
  :after evil
  :custom
  (evil-undo-system 'undo-fu))

(use-package undo-fu-session
  :ensure t
  :after undo-fu
  :custom
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (undo-fu-session-global-mode))

(use-package undo-tree
  :disabled
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

(use-package avy
  :ensure t)

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
    "n" '(:ignore t :which-key "Notes")
    "o" '(:ignore t :which-key "Org-mode")
    "p" '(:ignore t :which-key "Project")
    "q" '(:ignore t :which-key "Theme")
    "s" '(:ignore t :which-key "Scratch")
    "t" '(:ignore t :which-key "Tab")
    "w" '(:ignore t :which-key "Window")

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
    "ef" '(evil-avy-goto-char :which-key "Goto character")
    "es" '(evil-ex-sort :which-key "Sort")

    ;; File-related commands under `f'
    "fd" '(dired :which-key "Dired")
    "ff" '(find-file :which-key "Find file")
    "fF" '(consult-find :which-key "Find file")
    "fp" '(find-file-at-point :which-key "Find file at point")
    "fr" '(consult-ripgrep :which-key "Ripgrep")
    "fs" '(save-buffer :which-key "Save buffer")

    "SPC" '(execute-extended-command :which-key "M-x")
    "S-SPC" '(consult-mode-command :which-key "Mode command")

    ;; Editing-related commands
    ";" '(comment-dwim :which-key "Comment")
    "+" '(evil-numbers/inc-at-pt :which-key "Increase at point")
    "-" '(evil-numbers/dec-at-pt :which-key "Decrease at point")))

(use-package xref
  :general
  ("<mouse-8>" '(xref-go-back :which-key "Go back"))
  ("<mouse-9>" '(xref-go-forward :which-key "Go forward")))

(provide 'init-keybindings)
