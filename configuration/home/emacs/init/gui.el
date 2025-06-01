;;; -*- lexical-binding: t -*-

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
  (ring-bell-function 'ignore)
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

;; TODO: wait for https://gitlab.com/protesilaos/lin/-/issues/4.
(use-package lin
  :disabled
  :custom
  (lin-face 'lin-blue)
  (lin-mode-hooks '(dired-mode-hook
                    elfeed-search-mode-hook
                    git-rebase-mode-hook
                    ibuffer-mode-hook
                    ilist-mode-hook
                    ledger-report-mode-hook
                    log-view-mode-hook
                    magit-log-mode-hook
                    mu4e-headers-mode
                    notmuch-search-mode-hook
                    notmuch-tree-mode-hook
                    occur-mode-hook
                    org-agenda-mode-hook
                    tabulated-list-mode-hook)))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package ligature
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do
  ;; it per mode with `ligature-mode'.
  (global-ligature-mode t))

(provide 'init-gui)
