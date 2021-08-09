;; TODO: put some of this in early-init.el?

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

;; Hide menubar on all systems but darwin.
(use-package menu-bar
  :config
  (when (not (eq system-type 'darwin))
    (menu-bar-mode 0)))

(use-package emacs
  :custom
  (inhibit-startup-screen t)
  :config
  ;; Wrap lines.
  (global-visual-line-mode t)
  ;; Use `y' and `n' instead of `yes' and `no'.
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package advice
  :custom
  (ad-redefinition-action 'accept "Inhibit messages for redefined functions."))

(use-package hl-line
  :config
  (global-hl-line-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(provide 'init-gui)
