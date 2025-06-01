;;; -*- lexical-binding: t -*-

(require 'init-keybindings)

(use-package magit
  :ensure t
  :init
  (setq forge-add-default-bindings nil)
  :general
  (my-leader-def
    "gb" '(magit-blame :which-key "Git blame")
    "gg" '(magit-status :which-key "Magit")
    "gl" '(magit-log :which-key "Git log")))

(use-package forge
  :ensure t
  :after magit)

(use-package magit-todos
  :ensure t
  :after magit
  :hook
  (magit-mode . magit-todos-mode))

(use-package git-gutter
  :ensure t
  :demand t
  :custom
  (git-gutter:init-function 'git-gutter-fr:init)
  (git-gitter:view-diff-function 'git-gutter-fr:view-diff-infos)
  (git-gutter:clear-function 'git-gutter-fr:clear)
  (git-gutter:window-width -1)
  :config
  (global-git-gutter-mode)
  :general
  (my-leader-def
    "gn" '(git-gutter:next-hunk :which-key "Next hunk")
    "gp" '(git-gutter:previous-hunk :which-key "Previous hunk")))

(use-package git-gutter-fringe
  :ensure t
  :custom
  (fringes-outside-margins t)
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom))

(use-package browse-at-remote
  :ensure t
  :general
  (my-leader-def 
    "gr" '(browse-at-remote :which-key "Browse at remote")))

(provide 'init-git)
