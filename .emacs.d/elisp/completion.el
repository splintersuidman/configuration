(use-package company
  :demand t
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  :config
  (global-company-mode 1)
  :hook
  (c-mode . (lambda ()
              (delete 'company-semantic company-backends)))
  :bind
  (:map company-active-map
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)))

(use-package irony
  :disabled
  :hook
  (c-mode . irony-mode)
  (c++-mode . irony-mode)
  (objc-mode . irony-mode)
  (irony-mode . irony-cdb-autosetup-compile-options))

(use-package company-irony
  :disabled
  :after (company irony)
  :init
  (add-to-list 'company-backends 'company-irony))
