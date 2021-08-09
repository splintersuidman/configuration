(use-package project
  :init
  (defun splinter-project-root ()
    "Return the project root directory or ‘nil’ if there is no current project."
    (let ((project (project-current)))
      (when project
        (project-root project)))))

(provide 'init-project)
