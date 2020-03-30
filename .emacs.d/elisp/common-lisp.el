(use-package slime
  :mode "\\.lisp\\'"
  :init
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy)))
