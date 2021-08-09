(use-package window
  :custom
  (window-combination-resize t)
  (display-buffer-alist 
      '(("\\*\\(Help\\|WoMan\\).*"
         (display-buffer-in-side-window)
         (window-width . 0.30)
         (side . right)
         (slot . 0))
        ("\\*eldoc.*"
         (display-buffer-in-side-window)
         (window-width . 0.30)
         (side . right)
         (slot . 1))
        ("\\*.*\\(e?shell\\|v?term\\).*\\*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.30)))))

(provide 'init-window)
