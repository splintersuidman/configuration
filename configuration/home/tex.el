(require 'init-keybindings)

(use-package latex
  :ensure auctex
  :after general
  :mode ("\\.tex\\'" . latex-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  :init
  (setq-default TeX-PDF-mode t)
  (setq-default TeX-engine 'xetex)
  (setq-default TeX-master nil)
  :config
  ;; Revert TeX document buffer after compilation.
  (add-hook 'TeX-after-compilation-finished-functions
            'TeX-revert-document-buffer)
  (setf (alist-get 'output-pdf TeX-view-program-selection)
        '("PDF Tools"))

  (defun splinter-latex-add-left-right ()
    "Add \\left and \\right for the focused pair of brackets, braces, etc.

TODO: for escaped braces (\\{, \\}), this does not work."
    (interactive)
    (save-excursion
      (when-let* ((beg (ignore-errors (evil-jump-item)))
                  (end (ignore-errors (evil-jump-item)))
                  ;; Pair of things (brackets, braces, ...) from left to
                  ;; right.
                  (pair (if (< beg end)
                            (cons beg end)
                          (cons end beg)))
                  (left (car pair))
                  (right (cdr pair)))
        (goto-char left)
        (insert "\\left")
        ;; Add 5 because \left moves the point position.
        (goto-char (+ 5 right))
        (insert "\\right"))))
  :general
  (my-local-leader-def
    :keymaps 'LaTeX-mode-map
    "RET" 'TeX-insert-macro
    "a" 'TeX-command-run-all
    "b" 'TeX-command-buffer
    "c" 'TeX-command-master
    "e" 'LaTeX-environment
    "k" 'TeX-kill-job
    "l" 'TeX-recenter-output-buffer
    "n" 'TeX-normal-mode
    "p" 'splinter-latex-add-left-right
    "s" 'LaTeX-section
    "v" 'TeX-view))

(provide 'init-tex)
