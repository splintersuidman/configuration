(require 'init-icons)
(require 'init-keybindings)
(require 'init-project)

(use-package minibuffer
  :demand t
  :after general
  :custom
  (enable-recursive-minibuffers t)
  :general
  (:keymaps 'minibuffer-mode-map
   "C-u" 'delete-minibuffer-contents))

(use-package savehist
  :config
  (savehist-mode))

(use-package vertico
  :ensure t
  :demand t
  :after general
  :config
  (vertico-mode)
  :general
  (:keymaps 'vertico-map
   :states '(normal insert)
   "C-n" 'vertico-next
   "C-p" 'vertico-previous
   "C-y" 'evil-paste-after
   "TAB" 'vertico-insert
   "RET" 'vertico-exit)
  (:keymaps 'vertico-map
   :states 'normal
   "C-d" 'vertico-scroll-up
   "C-u" 'vertico-scroll-down
   "j" 'vertico-next
   "k" 'vertico-previous
   "gg" 'vertico-first
   "G" 'vertico-last))

(use-package vertico-directory
  :after vertico
  :general
  (:keymaps 'vertico-map
   :states '(normal insert)
   "C-<backspace>" 'vertico-directory-delete-word
   "M-DEL" 'vertico-directory-delete-word))

(use-package vertico-repeat
  :after vertico
  :hook
  (minibuffer-setup . vertico-repeat-save)
  :general
  (my-leader-def
    "r" '(vertico-repeat :which-key "Repeat minibuffer")))

(use-package orderless
  :ensure t
  :init
  (defun splinter-flex-dispatcher (pattern _index _total)
    "Use orderless-flex on a component if it ends with a tilde (~)."
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  :custom
  (completion-styles '(orderless))
  (read-file-name-completion-ignore-case t)
  (completion-category-overrides '((file (styles orderless partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism))
  (orderless-style-dispatchers '(splinter-flex-dispatcher)))

(use-package marginalia
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode))

(use-package all-the-icons-completion
  :ensure t
  :after all-the-icons
  :config
  (all-the-icons-completion-mode))

(use-package consult
  :ensure t
  :commands (consult-buffer consult-buffer-other-window consult-find consult-line consult-ripgrep)
  :after (general project)
  :custom
  (consult-project-root-function 'splinter-project-root)
  (consult-find-command "fd ARG . OPTS")
  (consult-ripgrep-command "rg --null --line-buffered --color=always --max-columns=500   --no-heading --line-number . -e ARG OPTS")
  :general
  (my-leader-def
    "/" '(consult-line :which-key "Search line")))

(use-package embark
  :ensure t
  :general
  (:keymaps 'override
   "C-," 'embark-act))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package company
  :disabled
  :ensure t
  :demand t
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  :config
  (global-company-mode)
  :general
  (:keymaps 'company-active-map
   "M-n" nil
   "M-p" nil
   "C-n" 'company-select-next
   "C-p" 'company-select-previous))

(use-package corfu
  :ensure t
  :demand t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 3)
  :config
  (corfu-global-mode)

  ;; See minad/corfu#12.
  (evil-make-overriding-map corfu-map)
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)

  (defun splinter-setup-corfu-for-minibuffer ()
    "Enable Corfu in the minibuffer, e.g. for
M-: (‘eval-expression’). Disable ‘corfu-auto’, since it will
interfere with Vertico for e.g. ‘find-file’."
    (make-local-variable 'corfu-auto)
    (setq corfu-auto nil)
    (corfu-mode))
  :hook
  (minibuffer-setup . splinter-setup-corfu-for-minibuffer)
  :general
  (:keymaps 'corfu-map
   "M-n" 'corfu-next
   "M-p" 'corfu-previous
   "C-n" 'corfu-next
   "C-p" 'corfu-previous))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions 'cape-keyword)
  (add-to-list 'completion-at-point-functions 'cape-file))

(use-package dabbrev
  ;; Swap M-/ and C-M-/.
  :general
  ("M-/" 'dabbrev-completion
   "C-M-/" 'dabbrev-expand))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  ;; See jdsmith/kind-icon#7. TODO: remove this when updated.
  (setf (alist-get 'file kind-icon-mapping)
        '("f" :icon "file-document-outline" :face font-lock-string-face))
  (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter))

(provide 'init-completion)
