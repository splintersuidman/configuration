(require 'init-keybindings)

(use-package project
  :after (general)
  :init
  (defun splinter-project-root ()
    "Return the project root directory or ‘nil’ if there is no current project."
    (when-let ((project (project-current)))
      (project-root project)))

  (cl-defmethod project-root ((project (head splinter-dot-project)))
    (cdr project))

  (defun splinter-project-try-dot-project (dir)
    "Try to find a .project file in ‘dir’ indicating a project directory."
    (let ((root (locate-dominating-file dir ".project")))
      (and root (cons 'splinter-dot-project root))))

  (add-to-list 'project-find-functions 'splinter-project-try-dot-project)
  :custom
  (project-switch-commands '((project-find-file "Find file" ?f)
                             (project-dired "Dired" ?d)
                             (project-eshell "Eshell" ?e)
                             (splinter-project-vterm "Vterm" ?v)
                             (project-execute-extended-command "Execute command" ?x)
                             (magit-project-status "Magit" ?g)))
  :general
  (my-leader-def
    "p!" '(project-shell-command :which-key "Shell command")
    "p&" '(project-async-shell-command :which-key "Async shell command")
    "pF" '(project-or-external-find-file :which-key "Or external find file")
    "pG" '(project-or-external-find-regexp :which-key "Or external find regexp")
    "pb" '(project-switch-to-buffer :which-key "Switch to buffer")
    "pc" '(project-compile :which-key "Compile")
    "pd" '(project-dired :which-key "Dired")
    "pe" '(project-eshell :which-key "Eshell")
    "pf" '(project-find-file :which-key "Find file")
    "pg" '(project-find-regexp :which-key "Find regexp")
    "pk" '(project-kill-buffers :which-key "Kill buffers")
    "pm" '(magit-project-status :which-key "Magit status")
    "pp" '(project-switch-project :which-key "Switch project")
    "ps" '(project-eshell :which-key "Shell")
    "pv" '(splinter-project-vterm :which-key "Vterm")
    "px" '(project-execute-extended-command :which-key "Execute command")))

(provide 'init-project)
