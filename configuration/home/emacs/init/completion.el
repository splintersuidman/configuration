;;; -*- lexical-binding: t -*-

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
  :config
  (defun splinter-vertico-repeat-history ()
    "Repeat selected Vertico session."
    (interactive)
    (vertico-repeat t))
  :general
  (my-leader-def
    "r" '(vertico-repeat :which-key "Repeat minibuffer")
    "R" '(splinter-vertico-repeat-history :which-key "Repeat minibuffer history")))

(use-package orderless
  :ensure t
  :init
  (defun splinter-flex-dispatcher (pattern _index _total)
    "Use ‘orderless-flex’ on a component if it ends with a
tilde (~)."
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  (defun splinter-invert-dispatcher (pattern _index _total)
    "Use ‘orderless-without-literal’ on a component if it ends with
an exclamation point (!)."
    (when (string-suffix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 0 -1))))
  :custom
  (completion-styles '(orderless))
  (read-file-name-completion-ignore-case t)
  (completion-category-overrides '((file (styles orderless partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism))
  (orderless-style-dispatchers '(splinter-flex-dispatcher splinter-invert-dispatcher)))

(use-package marginalia
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode))

(use-package all-the-icons-completion
  :disabled
  :ensure t
  :after all-the-icons
  :config
  (all-the-icons-completion-mode))

(use-package nerd-icons-completion
  :ensure t
  :after nerd-icons
  :config
  (nerd-icons-completion-mode))

(use-package consult
  :ensure t
  :commands (consult-buffer consult-buffer-other-window consult-find consult-line consult-ripgrep)
  :after (general project)
  :config
  (defun splinter-consult-line ()
    "Search for matching line with `consult-line', with contents of region as
initial input if active."
    (interactive)
    (consult-line (if (region-active-p)
                      (buffer-substring (region-beginning) (region-end))
                    nil)))
  :custom
  (consult-project-root-function 'splinter-project-root)
  (consult-find-command "fd ARG . OPTS")
  (consult-ripgrep-command "rg --null --line-buffered --color=always --max-columns=500   --no-heading --line-number . -e ARG OPTS")
  :general
  (my-leader-def
    "/" '(splinter-consult-line :which-key "Search line"))
  (my-local-leader-def
   :keymaps 'minibuffer-mode-map
   "r" 'consult-history))

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
  (corfu-quit-at-boundary nil)
  :config
  (global-corfu-mode)

  ;; See minad/corfu#12.
  (evil-make-overriding-map corfu-map)

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

;; Use corfu-terminal because Cosmic tiles child frames.
;; TODO: find a way to make Cosmic not tile child frames.
(use-package corfu-terminal
  :ensure t
  :functions corfu-terminal-mode
  :custom
  (corfu-terminal-disable-on-gui nil)
  :config
  (corfu-terminal-mode))

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
  :after (corfu nerd-icons)
  :custom
  ;; (kind-icon-default-face 'corfu-default)  ; for all-the-icons
  (kind-icon-use-icons nil)
  (kind-icon-mapping
   `(
     (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
     (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
     (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
     (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
     (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
     (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
     (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
     (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
     (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
     (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
     (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
     (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
     (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
     (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
     (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
     (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
     (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
     (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
     (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
     (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
     (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
     (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
     (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
     (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
     (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
     (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
     (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
     (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
     (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
     (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
     (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
     (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
     (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
     (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
     (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
     (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)))
  :config
  (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter))

(provide 'init-completion)
