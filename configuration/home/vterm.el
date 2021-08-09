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
  :general
  (my-leader-def
    "v" '(splinter-project-vterm :which-key "Terminal")))

(provide 'init-vterm)
