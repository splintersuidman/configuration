;;; -*- lexical-binding: t -*-

(require 'init-keybindings)
(require 'init-project)

(use-package vterm
  :ensure t
  :after (project)
  :init
  (defun splinter-project-vterm ()
    "Start ‘vterm’ in the current project directory or in ‘default-directory’ if
  there is no current project."
    (interactive)
    (defvar vterm-buffer-name)
    (let* ((default-directory (or (splinter-project-root) default-directory))
           (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
           (vterm-buffer (get-buffer vterm-buffer-name)))
      (if (and vterm-buffer (not current-prefix-arg))
          (pop-to-buffer vterm-buffer)
        (vterm t))))
  (add-hook 'vterm-exit-functions
            (lambda (_buffer _event)
              (unless (one-window-p)
                (delete-window))))
  :general
  (my-leader-def
    "v" '(splinter-project-vterm :which-key "Terminal"))
  (my-leader-def
    :keymaps '(vterm-mode-map)
    "" nil)
  (my-local-leader-def
    :keymaps '(vterm-mode-map)
    "l" '(vterm-clear-scrollback :which-key "Clear scrollback")
    "n" '(vterm-next-prompt :which-key "Next prompt")
    "p" '(vterm-previous-prompt :which-key "Previous prompt")
    "r" '(vterm-reset-cursor-point :which-key "Reset cursor point")
    "t" '(vterm-copy-mode :which-key "Copy mode")))

(provide 'init-vterm)
