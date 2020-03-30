(load-file
 (let ((coding-system-for-read 'utf-8))
   (shell-command-to-string "agda-mode locate")))

(setq auto-mode-alist
   (append
     '(("\\.agda\\'" . agda2-mode)
       ("\\.lagda.md\\'" . agda2-mode)
       ("\\.lagda.org\\'" . agda2-mode)
       ("\\.lagda.tex\\'" . agda2-mode))
     auto-mode-alist))

(add-hook 'agda2-mode-hook
          (lambda ()
            (evil-leader/set-key-for-mode 'agda2-mode
              "c," 'agda2-goal-and-context
              "c." 'agda2-goal-and-context-and-inferred
              "c;" 'agda2-goal-context-and-checked
              "c=" 'agda2-show-constraints
              "c?" 'agda2-show-goals
              "ca" 'agda2-auto-maybe-all
              "cb" 'agda2-previous-goal
              "cc" 'agda2-make-case
              "cd" 'agda2-infer-type-maybe-toplevel
              "ce" 'agda2-show-context
              "cf" 'agda2-next-goal
              "ch" 'agda2-helper-function-type
              "cl" 'agda2-load
              "cn" 'agda2-compute-normalised-maybe-toplevel
              "co" 'agda2-module-contents-maybe-toplevel
              "cr" 'agda2-refine
              "cs" 'agda2-solve-maybe-all
              "ct" 'agda2-goal-type
              "cw" 'agda-why-in-scope-maybe-toplevel
              "cxa" 'agda2-abort
              "cxc" 'agda2-compile
              "cxd" 'agda2-remove-annotations
              "cxh" 'agda2-remove-implicit-arguments
              "cxl" 'agda2-load
              "cxq" 'agda2-quit
              "cxr" 'agda2-restart
              "cxs" 'agda2-set-program-version
              "cx;" 'agda2-comment-dwim-rest-of-buffer
              "cz" 'agda2-search-about-toplevel)))

;; TODO: use `use-package' for `agda-mode'
