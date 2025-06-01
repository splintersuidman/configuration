;;; -*- lexical-binding: t -*-

(require 'init-keybindings)

(use-package agda2-mode
  :custom
  (agda-input-user-translations '(("eq" "＝")
                                  ("==" "＝")))
  :load-path
  (lambda ()
    (let ((coding-system-for-read 'utf-8))
      (file-name-directory (shell-command-to-string "agda-mode locate"))))
  :mode "\\.agda\\'" "\\.lagda.md\\'" "\\.lagda.org\\'" "\\.lagda.tex\\'"
  :init
  (defun splinter-agda-mode-set-faces (&optional force)
    "Make agda-mode faces inherit from font-lock."
    (when (or force (featurep 'agda2-mode))
      (set-face-attribute 'agda2-highlight-datatype-face nil :inherit font-lock-type-face :foreground nil)
      (set-face-attribute 'agda2-highlight-field-face nil :inherit font-lock-function-name-face :foreground nil)
      (set-face-attribute 'agda2-highlight-function-face nil :inherit font-lock-function-name-face :foreground nil)
      (set-face-attribute 'agda2-highlight-inductive-constructor-face nil :inherit font-lock-type-face :foreground nil)
      (set-face-attribute 'agda2-highlight-keyword-face nil :inherit font-lock-keyword-face :foreground nil)
      (set-face-attribute 'agda2-highlight-module-face nil :inherit font-lock-type-face :foreground nil)
      (set-face-attribute 'agda2-highlight-number-face nil :inherit font-lock-type-face :foreground nil)
      (set-face-attribute 'agda2-highlight-operator-face nil :inherit font-lock-variable-name-face :foreground nil)
      (set-face-attribute 'agda2-highlight-postulate-face nil :inherit font-lock-type-face :foreground nil)
      (set-face-attribute 'agda2-highlight-primitive-face nil :inherit font-lock-function-name-face :foreground nil)
      (set-face-attribute 'agda2-highlight-primitive-type-face nil :inherit font-lock-type-face :foreground nil)
      (set-face-attribute 'agda2-highlight-record-face nil :inherit font-lock-type-face :foreground nil)
      (set-face-attribute 'agda2-highlight-symbol-face nil :inherit font-lock-variable-name-face :foreground nil)))

  (defun evil-goto-definition-annotation (_string _position)
    (annotation-goto-indirect (point)))
  :hook
  (agda2-mode . splinter-agda-mode-set-faces)
  (after-load-theme . splinter-agda-mode-set-faces)
  (agda2-mode . (lambda ()
                  (setq-local evil-goto-definition-functions
                              (cons 'evil-goto-definition-annotation evil-goto-definition-functions))))
  (agda2-mode . (lambda () (setq-local evil-shift-width 2)))
  :general
  (my-local-leader-def
    :keymaps 'agda2-mode-map
    "," '(agda2-goal-and-context :which-key "Goal and context")
    "." '(agda2-goal-and-context-and-inferred :which-key "Goal, context and inferred")
    ";" '(agda2-goal-and-context-and-checked :which-key "Goal, context and checked")
    "=" '(agda2-show-constraints :which-key "Show constraints")
    "?" '(agda2-show-goals :which-key "Show goals")
    "a" '(agda2-auto-maybe-all :which-key "Auto solve")
    "b" '(agda2-previous-goal :which-key "Previous goal")
    "c" '(agda2-make-case :which-key "Case split")
    "d" '(agda2-infer-type-maybe-toplevel :which-key "Infer type")
    "e" '(agda2-show-context :which-key "Show context")
    "f" '(agda2-next-goal :which-key "Next goal")
    "h" '(agda2-helper-function-type :which-key "Helper function type")
    "l" '(agda2-load :which-key "Load")
    "n" '(agda2-compute-normalised-maybe-toplevel :which-key "Normal form")
    "o" '(agda2-module-contents-maybe-toplevel :which-key "Module contents")
    "r" '(agda2-refine :which-key "Refine")
    "s" '(agda2-solve-maybe-all :which-key "Solve all")
    "t" '(agda2-goal-type :which-key "Goal type")
    "w" '(agda-why-in-scope-maybe-toplevel :which-key "Why in scope")
    "xa" '(agda2-abort :which-key "Abort")
    "xc" '(agda2-compile :which-key "Compile")
    "xd" '(agda2-remove-annotations :which-key "Remove annotations")
    "xh" '(agda2-remove-implicit-arguments :which-key "Remove implicit arguments")
    "xl" '(agda2-load :which-key "Load")
    "xq" '(agda2-quit :which-key "Quit")
    "xr" '(agda2-restart :which-key "Restart")
    "xs" '(agda2-set-program-version :which-key "Set version")
    "x;" '(agda2-comment-dwim-rest-of-buffer :which-key "Comment rest of buffer")
    "z" '(agda2-search-about-toplevel :which-key "Search")))

(provide 'init-agda)
