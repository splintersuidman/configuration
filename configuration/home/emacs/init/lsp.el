;;; -*- lexical-binding: t -*-

(require 'init-icons)
(require 'init-keybindings)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (add-hook 'lsp-mode-hook
            (lambda ()
              (setq-local evil-goto-definition-functions '(evil-goto-definition-xref))))
  :custom
  (lsp-auto-guess-root t)
  (lsp-idle-delay 0.05)
  (lsp-enable-suggest-server-download nil)
  (lsp-file-watch-threshold 2048)
  :config
  (defun splinter-lsp-workspace-shutdown-all ()
    (interactive)
    (dolist (workspace (lsp-workspaces))
      (lsp-workspace-shutdown workspace)))
  :general
  (my-leader-def
    "la" '(lsp-execute-code-action :which-key "Code actions")
    "leb" '(flymake-show-buffer-diagnostics :which-key "Diagnostics")
    "len" '(flymake-goto-next-error :which-key "Next error")
    "lep" '(flymake-goto-prev-error :which-key "Previous error")
    "leq" '(lsp-workspace-shutdown :which-key "Shutdown")
    "lf" '(lsp-format-buffer :which-key "Format buffer")
    "lF" '(lsp-format-region :which-key "Format region")
    "ll" '(lsp :which-key "Start LSP")
    "lq" '(splinter-lsp-workspace-shutdown-all :which-key "Shutdown all")
    "lr" '(lsp-rename :which-key "Rename")
    "lR" '(lsp-workspace-restart :which-key "Restart")
    "lt" '(lsp-goto-type-definition :which-key "Type definition")))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-headerline-breadcrumb-icons-enable nil)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-imenu-buffer-position 'left)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  :general
  ;; Evil keybindings for lsp-ui-peek
  (:keymaps 'lsp-ui-peek-mode-map
   "h" 'lsp-ui-peek--select-prev-file
   "j" 'lsp-ui-peek--select-next
   "k" 'lsp-ui-peek--select-prev
   "l" 'lsp-ui-peek--select-next-file)
  (:keymaps 'lsp-ui-imenu-mode-map
   "<tab>" 'lsp-ui-imenu--view
   "<return>" 'lsp-ui-imenu--visit)
  (my-leader-def
    "lm" '(lsp-ui-imenu :which-key "Imenu")
    "lp" '(:ignore t :which-key "Peek")
    "lpd" '(lsp-ui-peek-find-definitions :which-key "Find definitions")
    "lpi" '(lsp-ui-peek-find-implementation :which-key "Find implementation")
    "lpr" '(lsp-ui-peek-find-references :which-key "Find references")
    "lps" '(lsp-ui-peek-find-workspace-symbol :which-key "Find symbol")))

;; Install treemacs for the lsp-mode call hierarchy.
(use-package treemacs
  :ensure t
  :after lsp-mode)

(use-package treemacs-nerd-icons
  :ensure t
  :after (treemacs nerd-icons)
  :config
  (treemacs-nerd-icons-config))

(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs nerd-icons)
  :general
  (my-leader-def
    "lh" '(lsp-treemacs-call-hierarchy :which-key "Call hierarchy"))
  :custom
  (lsp-treemacs-theme "lsp-nerd-icons")
  :config
  ;; See <https://github.com/emacs-lsp/lsp-treemacs/issues/180>.
  (treemacs-create-theme "lsp-nerd-icons"
    :config
    (progn
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-repo" :face 'nerd-icons-blue))
       :extensions (root))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_boolean" :face 'nerd-icons-lblue))
       :extensions (boolean-data))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_class" :face 'nerd-icons-orange))
       :extensions (class))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_color"))
       :extensions (color-palette))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_constant"))
       :extensions (constant))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_file"))
       :extensions (document))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_misc" :face 'nerd-icons-orange))
       :extensions (enumerator))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_enum_member" :face 'nerd-icons-lblue))
       :extensions (enumitem))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_event" :face 'nerd-icons-orange))
       :extensions (event))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_field" :face 'nerd-icons-lblue))
       :extensions (field))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_misc"))
       :extensions (indexer))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_keyword"))
       :extensions (intellisense-keyword))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_interface" :face 'nerd-icons-lblue))
       :extensions (interface))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_variable" :face 'nerd-icons-lblue))
       :extensions (localvariable))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_method" :face 'nerd-icons-purple))
       :extensions (method))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_namespace" :face 'nerd-icons-lblue))
       :extensions (namespace))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_numeric"))
       :extensions (numeric))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_operator"))
       :extensions (operator))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_property"))
       :extensions (property))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_snippet"))
       :extensions (snippet))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_string"))
       :extensions (string))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_structure" :face 'nerd-icons-orange))
       :extensions (structure))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_snippet"))
       :extensions (template))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-chevron_right" :face 'nerd-icons-dsilver))
       :extensions (collapsed) :fallback "+")
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-chevron_down" :face 'nerd-icons-dsilver))
       :extensions (expanded) :fallback "-")
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-file_binary" :face 'nerd-icons-dsilver))
       :extensions (classfile))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-blue))
       :extensions (default-folder-opened))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-blue))
       :extensions (default-folder))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-green))
       :extensions (default-root-folder-opened))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-green))
       :extensions (default-root-folder))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-file_binary" :face 'nerd-icons-dsilver))
       :extensions ("class"))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-file_zip" :face 'nerd-icons-dsilver))
       :extensions (file-type-jar))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-dsilver))
       :extensions (folder-open))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-dsilver))
       :extensions (folder))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-orange))
       :extensions (folder-type-component-opened))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-orange))
       :extensions (folder-type-component))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-green))
       :extensions (folder-type-library-opened))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-green))
       :extensions (folder-type-library))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-pink))
       :extensions (folder-type-maven-opened))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-pink))
       :extensions (folder-type-maven))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-orange))
       :extensions (folder-type-package-opened))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-orange))
       :extensions (folder-type-package))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-add" :face 'nerd-icons-dsilver))
       :extensions (icon-create))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-list_flat" :face 'nerd-icons-dsilver))
       :extensions (icon-flat))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-symbol_class" :face 'nerd-icons-blue))
       :extensions (icon-hierarchical))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-link" :face 'nerd-icons-dsilver))
       :extensions (icon-link))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-refresh" :face 'nerd-icons-dsilver))
       :extensions (icon-refresh))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-faicon "nf-fa-unlink" :face 'nerd-icons-dsilver))
       :extensions (icon-unlink))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-devicon "nf-dev-java" :face 'nerd-icons-orange))
       :extensions (jar))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-library" :face 'nerd-icons-green))
       :extensions (library))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-folder_opened" :face 'nerd-icons-lblue))
       :extensions (packagefolder-open))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-folder" :face 'nerd-icons-lblue))
       :extensions (packagefolder))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-archive" :face 'nerd-icons-dsilver))
       :extensions (package))
      (treemacs-create-icon
       :icon (format "%s " (nerd-icons-codicon "nf-cod-repo" :face 'nerd-icons-blue))
       :extensions (java-project)))))

(provide 'init-lsp)
