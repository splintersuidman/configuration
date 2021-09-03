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
   "RET" 'vertico-exit)
  (:keymaps 'vertico-map
   :states 'normal
   "C-d" 'vertico-scroll-up
   "C-u" 'vertico-scroll-down
   "j" 'vertico-next
   "k" 'vertico-previous
   "gg" 'vertico-first
   "G" 'vertico-last))

(use-package orderless
  :ensure t
  :init
  (defun splinter-flex-dispatcher (pattern _index _total)
    "Use orderless-flex on a component if it ends with a tilde (~)."
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  :custom
  (completion-styles '(orderless))
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
  :general
  (:keymaps 'corfu-map
    "M-n" 'corfu-next
    "M-p" 'corfu-previous
    ;; TODO: this does not work because C-n executes ‘evil-complete-next’ and
    ;; C-p executes ‘evil-complete-previous’. See also
    ;; <https://github.com/minad/corfu/issues/12>.
    "C-n" 'corfu-next
    "C-p" 'corfu-previous))

(provide 'init-completion)
