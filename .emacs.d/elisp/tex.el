(use-package tex-mode
  :defer t
  :init
  (setq TeX-engine 'xetex)
  :config
  (evil-leader/set-key-for-mode 'tex-mode
    "cc" 'TeX-command-master
    "cv" 'TeX-view))
