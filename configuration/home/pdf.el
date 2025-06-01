;;; -*- lexical-binding: t -*-

(require 'init-keybindings)

(use-package pdf-tools
  :ensure t
  :after general
  :functions pdf-tools-install
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  ;; NOTE: see https://github.com/politza/pdf-tools/issues/528#issuecomment-564773133.
  (require 'pdf-annot)
  (require 'pdf-history)
  (require 'pdf-info)
  (require 'pdf-isearch)
  (require 'pdf-links)
  (require 'pdf-misc)
  (require 'pdf-occur)
  (require 'pdf-tools)
  (require 'pdf-util)
  (require 'pdf-view)
  (pdf-tools-install :no-query)
  :general
  (my-leader-def
    :keymaps 'pdf-view-mode-map
    ;; Unbind leader key, as pdf-tools binds SPC.
    "" nil)
  (my-local-leader-def
    :keymaps 'pdf-view-mode-map
    "r" 'pdf-view-midnight-minor-mode))

(provide 'init-pdf)
