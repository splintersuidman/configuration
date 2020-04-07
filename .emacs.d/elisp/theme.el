(use-package spacemacs-theme
  :disabled
  :init
  (setq spacemacs-theme-comment-bg nil)
  (setq spacemacs-theme-comment-italic t)
  (load-theme 'spacemacs-dark t))

(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(load-theme 'green-dark t)

(defvar my/themes
  (list 'green-light 'green-dark)
  "A list of themes that can be cycled through with
  `MY/SWITCH-THEME'.")

(defun my/switch-theme ()
  (interactive)
  (let ((next (pop my/themes)))
    (setq my/themes (append my/themes (list next)))
    (load-theme next t)
    (when (featurep 'spaceline)
      (powerline-reset))))
