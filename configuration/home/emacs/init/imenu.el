;;; -*- lexical-binding: t -*-

(require 'init-keybindings)

(use-package imenu
  :after general
  :custom
  (imenu-max-item-length nil)
  :general
  (my-leader-def
    "m" '(consult-imenu :which-key "Imenu")
    "M" '(consult-imenu-multi :which-key "Project imenu")))

(provide 'init-imenu)
