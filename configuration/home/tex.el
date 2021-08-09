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
  :general
  (my-local-leader-def
    :keymaps 'LaTeX-mode-map
    "c" 'TeX-command-master
    "e" 'LaTeX-environment
    "s" 'LaTeX-section
    "v" 'TeX-view))

(provide 'init-tex)
