(use-package ido
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'always)
  (setq ido-everywhere t)
  :config
  (ido-mode))

(use-package ido-vertical-mode
  :after ido
  :init
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :config
  (ido-vertical-mode))
