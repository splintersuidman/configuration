(use-package magit
  :defer t
  :init
  (evil-leader/set-key
    "gg" 'magit-status))

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode))

(use-package evil-magit)

(use-package git-gutter
  :config
  (global-git-gutter-mode))
