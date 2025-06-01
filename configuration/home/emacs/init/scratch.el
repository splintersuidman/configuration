;;; -*- lexical-binding: t -*-

(require 'init-keybindings)

(use-package scratch
  :ensure t
  :after (general)
  :functions (scratch--create scratch--list-modes)
  :config
  (defun splinter-scratch-query-mode ()
    (intern (concat (completing-read
                     "Mode: " (scratch--list-modes)
                     nil t nil scratch--history)
                    "-mode")))

  (defun splinter-scratch (mode)
    (let* ((name (format "*%s-scratch*" (replace-regexp-in-string "-mode$" "" (symbol-name mode))))
           (buf (get-buffer name)))
      (pop-to-buffer (if (bufferp buf)
                         buf
                       (scratch--create mode name)))))

  (defun splinter-scratch-query ()
    (interactive)
    (splinter-scratch (splinter-scratch-query-mode)))

  (defun splinter-scratch-current-mode ()
    (interactive)
    (splinter-scratch major-mode))
  :general
  (my-leader-def
    "ss" '(splinter-scratch-query :which-key "Scratch")
    "sS" '(splinter-scratch-current-mode :which-key "Scratch current mode")))

(provide 'init-scratch)
