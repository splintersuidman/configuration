;;; -*- lexical-binding: t -*-

(require 'init-keybindings)

(use-package just-mode
  :ensure t
  :mode ("justfile" . just-mode)
  :after general
  :init
  (defun just-run-command (command)
    "Run a `just' command."
    (interactive
     (list (with-temp-buffer
             (call-process "just" nil t nil "--dump" "--dump-format" "json")
             (beginning-of-buffer)
             (completing-read "just " (hash-table-keys (gethash "recipes" (json-parse-buffer)))))))
    (shell-command (concat "just " command)))
  :general
  (my-local-leader-def
    :keymaps 'just-mode-map
    "c" 'just-run-command
    "f" 'just-format-buffer))

(use-package just-mode
  :after eglot
  :init
  (add-to-list 'eglot-server-programs '(just-mode . ("just-lsp"))))

(provide 'init-just)
