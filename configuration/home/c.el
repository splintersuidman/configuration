;;; -*- lexical-binding: t -*-

(use-package cc-mode
  :mode
  ("\\.c\\'" . c-mode)
  ("\\.h\\'" . c-mode)
  ("\\.cpp\\'" . c++-mode)
  ("\\.cc\\'" . c++-mode)
  ("\\.hpp\\'" . c++-mode)
  :custom
  (c-basic-offset 4)
  (c-default-style "k&r")

(use-package cc-mode
  :after eglot
  :init
  (add-to-list 'eglot-server-programs '(c-mode . ("ccls")))
  (add-to-list 'eglot-server-programs '(c++-mode . ("ccls")))))

(provide 'init-c)
