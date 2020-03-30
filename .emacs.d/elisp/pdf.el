(use-package pdf-tools
  :config
  (pdf-loader-install)
  (evil-leader/set-key-for-mode 'pdf-view-mode
    "cr" 'pdf-view-midnight-minor-mode))
