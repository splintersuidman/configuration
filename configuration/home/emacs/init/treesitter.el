;;; -*- lexical-binding: t -*-

(require 'init-keybindings)

(use-package tree-sitter-langs
  :ensure t)

(use-package evil-textobj-tree-sitter
  :ensure t
  :after (evil general)
  :general
  (:keymaps 'evil-outer-text-objects-map
   "/" (evil-textobj-tree-sitter-get-textobj "comment.outer")
   "c" (evil-textobj-tree-sitter-get-textobj "class.outer")
   "e" (evil-textobj-tree-sitter-get-textobj "entry.outer")
   "f" (evil-textobj-tree-sitter-get-textobj "function.outer")
   "i" (evil-textobj-tree-sitter-get-textobj "conditional.outer")
   "l" (evil-textobj-tree-sitter-get-textobj "loop.outer")
   "p" (evil-textobj-tree-sitter-get-textobj "parameter.outer"))
  (:keymaps 'evil-inner-text-objects-map
   "/" (evil-textobj-tree-sitter-get-textobj "comment.inner")
   "c" (evil-textobj-tree-sitter-get-textobj "class.inner")
   "e" (evil-textobj-tree-sitter-get-textobj "entry.inner")
   "f" (evil-textobj-tree-sitter-get-textobj "function.inner")
   "i" (evil-textobj-tree-sitter-get-textobj "conditional.inner")
   "l" (evil-textobj-tree-sitter-get-textobj "loop.inner")
   "p" (evil-textobj-tree-sitter-get-textobj "parameter.inner")))

(provide 'init-treesitter)
