;; TODO: early-init
(setq user-emacs-directory "~/.emacs.d/")
(setq user-init-file (expand-file-name "init.el" user-emacs-directory))

(setq package-enable-at-startup nil)

(eval-when-compile
  (require 'use-package)
  (setq use-package-verbose nil))

(add-to-list 'load-path (expand-file-name "init/" user-emacs-directory))

;; Load enabled modules.
(mapc 'require
      (with-temp-buffer
	(insert-file-contents (expand-file-name "init-features" user-emacs-directory))
	(read (current-buffer))))

(provide 'init)
