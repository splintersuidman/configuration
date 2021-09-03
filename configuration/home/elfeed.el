(require 'init-keybindings)

(use-package elfeed
  :ensure t
  :after general
  :commands (elfeed)
  :init
  (defun splinter-elfeed-set-faces (&optional force)
    "Set elfeed faces for base16-themes."
    (when (or force
              (and (featurep 'elfeed)
                   (-any? (lambda (theme)
                            (string-prefix-p "base16-" (symbol-name theme)))
                          custom-enabled-themes)))
      (message "Setting fonts")
      (set-face-attribute 'elfeed-search-feed-face nil :inherit font-lock-function-name-face :foreground nil)
      (set-face-attribute 'elfeed-search-tag-face nil :inherit font-lock-type-face :foreground nil)
      (set-face-attribute 'elfeed-search-date-face nil :inherit font-lock-variable-name-face :foreground nil)))
  :config
  (defun splinter-elfeed-mpv (entry)
    "Open the link of the currently selected item in mpv."
    (interactive (list (elfeed-search-selected :ignore-region)))
    (when (elfeed-entry-p entry)
      (elfeed-untag entry 'unread)
      (elfeed-search-update-entry entry)
      (make-process :name (elfeed-entry-title entry)
                    :command (list "mpv" (elfeed-entry-link entry)))))
  :general
  (my-leader-def
    :keymaps 'elfeed-search-mode-map
    ;; Unbind leader key, as elfeed binds SPC.
    "" nil)
  (my-local-leader-def
    :keymaps 'elfeed-search-mode-map
    "u" 'elfeed-update
    "v" 'splinter-elfeed-mpv)
  :hook
  (after-load-theme . splinter-elfeed-set-faces))

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org))

(provide 'init-elfeed)
