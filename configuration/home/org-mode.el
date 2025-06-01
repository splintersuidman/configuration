;;; -*- lexical-binding: t -*-

(require 'init-keybindings)

(use-package org
  :ensure t
  :demand t
  :after general
  :custom
  (org-log-done 'time)
  (org-catch-invisible-edits 'show-and-error)
  (org-list-indent-offset 0)
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
  (org-startup-indented nil)
  (sentence-end-double-space nil)
  (org-hide-emphasis-markers t)
  (org-highlight-latex-and-related '(latex script))

  (calendar-week-start-day 1)
  (calendar-day-name-array ["zondag" "maandag" "dinsdag" "woensdag"
                            "donderdag" "vrijdag" "zaterdag"])
  (calendar-month-name-array ["januari" "februari" "maart" "april" "mei"
                              "juni" "juli" "augustus" "september"
                              "oktober" "november" "december"])
  :config
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-modules 'org-habit)

  (defun splinter-ncatlab-follow (title arg)
    "Follow ncatlab link."
    (browse-url (concat "https://ncatlab.org/nlab/show/" title) arg))

  (org-link-set-parameters "ncatlab"
                           :follow 'splinter-ncatlab-follow)

  (org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))
  :hook
  (org-agenda-mode . auto-save-mode)
  (auto-save-mode . org-save-all-org-buffers)
  :general
  (my-leader-def
    "oa" '(org-agenda :which-key "Agenda menu")
    "on" '((lambda ()
             (interactive)
             (org-agenda nil "n"))
           :which-key "Agenda")
    "oo" '((lambda ()
             (interactive)
             (org-agenda nil "n")
             (delete-other-windows))
           :which-key "Full-frame agenda"))
  (my-local-leader-def
    :keymaps 'org-mode-map
    "!" 'org-time-stamp-inactive
    "#" 'org-update-statistics-cookies
    "$" 'org-archive-subtree
    "%" 'org-mark-ring-push
    "'" 'org-edit-special
    "*" 'org-ctrl-c-star
    "+" 'org-table-sum
    "," 'org-priority
    "-" 'org-ctrl-c-minus
    "." 'org-time-stamp
    "/" 'org-sparse-tree
    ":" 'org-toggle-fixed-width
    ";" 'org-toggle-comment
    "<" 'org-date-from-calendar
    "=" 'org-table-eval-formula
    ">" 'org-goto-calendar
    "?" 'org-table-field-info
    "@" 'org-mark-subtree
    "[" 'org-agenda-file-to-front
    "\\" 'org-match-sparse-tree
    "]" 'org-remove-file
    "^" 'org-sort
    "`" 'org-table-edit-field
    "{" 'org-table-toggle-formula-debugger
    "|" 'org-table-create-or-convert-from-region
    "}" 'org-table-toggle-coordinate-overlays
    "~" 'org-table-create-with-table.el
    ;; "c*" 'org-list-make-subtree
    ;; "c," 'org-insert-structure-template
    ;; "c<" 'outline-promote
    ;; "c>" 'outline-demote
    ;; "c^" 'org-up-element
    ;; "c_" 'org-down-element
    "a" 'org-attach
    "b" 'org-backward-heading-same-level
    "c" 'org-ctrl-c-ctrl-c
    "d" 'org-deadline
    "e" 'org-export-dispatch
    "f" 'org-forward-heading-same-level
    "j" 'org-goto
    "k" 'org-kill-note-or-show-branches
    "l" 'org-insert-link
    "n" 'outline-next-visible-heading
    "o" 'org-open-at-point
    "p" 'outline-previous-visible-heading
    "q" 'org-set-tags-command
    "r" 'org-reveal
    "s" 'org-schedule
    "t" 'org-todo
    "u" 'outline-up-heading
    ;; "cv" TODO
    "w" 'org-refile
    "x!" 'org-reload
    "x," 'org-timer-pause-or-continue
    "x-" 'org-timer-item
    "x." 'org-timer
    "x0" 'org-timer-start
    "x;" 'org-timer-set-timer
    "x<" 'org-agenda-set-restriction-lock
    "x>" 'org-agenda-remove-restriction-lock
    "xA" 'org-archive-to-archive-sibling
    "xE" 'org-inc-effort
    "xG" 'org-feed-goto-inbox
    "xI" 'org-info-find-node
    "xP" 'org-set-property-and-value
    "x[" 'org-reftex-citation
    "x\\" 'org-toggle-pretty-entities
    "x_" 'org-timer-stop
    "xa" 'org-toggle-archive-tag
    "xb" 'org-tree-to-indirect-buffer
    "xc" 'org-clone-subtree-with-time-shift
    "xd" 'org-insert-drawer
    "xe" 'org-set-effort
    "xf" 'org-footnote-action
    "xg" 'org-feed-update-all
    "xi" 'org-columns-insert-dblock
    "xo" 'org-toggle-ordered-property
    "xp" 'org-set-property
    "xq" 'org-toggle-tags-groups
    "xv" 'org-copy-visible
    "x TAB" 'org-clock-in
    "xA" 'org-archive-subtree
    "xB" 'org-toggle-checkbox
    "xC" 'org-columns
    "xD" 'org-clock-display
    "xE" 'org-clock-modify-effort-estimate
    "xF" 'org-emphasize
    "xJ" 'org-clock-goto
    "xL" 'org-toggle-latex-fragment
    "xN" 'org-next-link
    "xO" 'org-clock-out
    "xP" 'org-previous-link
    "xQ" 'org-clock-cancel
    "xR" 'org-clock-report
    "xS" 'org-archive-subtree
    "xT" 'org-toggle-time-stamp-overlays
    "xU" 'org-dblock-update
    "xV" 'org-toggle-inline-images
    "xW" 'org-cut-special
    "y" 'org-evaluate-time-range)

  ;; Unbind leader key.
  (my-leader-def
    :keymaps 'org-agenda-mode-map
    "" nil)
  (my-local-leader-def
    :keymaps 'org-agenda-mode-map
    "," 'org-agenda-priority
    "a" 'org-attach
    "d" 'org-agenda-deadline
    "n" 'org-agenda-next-date-line
    "o" 'org-agenda-open-link
    "p" 'org-agenda-next-date-line
    "q" 'org-agenda-set-tags
    "s" 'org-agenda-schedule
    "t" 'org-agenda-todo
    "w" 'org-agenda-refile
    ;; TODO: add more keys under cx.
    "x TAB" 'org-agenda-clock-in
    "xo" 'org-agenda-clock-out
    "z" 'org-agenda-add-note

    ;; TODO: evil-org's [ and ] seem to be broken. C-h C-m says
    ;; that the bindings are shadowed, but I cannot find by
    ;; what. Version of 2020-01-01 does work, version of
    ;; 2020-09-22 does not.
    "[" 'org-agenda-earlier
    "]" 'org-agenda-later)

  ;; TODO: does not seem to work.
  (my-local-leader-def
    :keymaps 'org-src-mode-map
    "'" 'org-edit-src-exit
    "k" 'org-edit-src-abort))

;; Org-mode and embark
(use-package org
  :after embark
  :config
  (defun splinter-embark-org-link ()
    "Target the Org-mode link at point."
    (when-let ((link (org-element-lineage (org-element-context) '(link) t)))
      (let* ((beg (org-element-property :contents-begin link))
             (end (org-element-property :contents-end link)))
        (cons 'org-link
              (if (and beg end)
                  (cons (buffer-substring-no-properties beg end) (cons beg end))
                (org-element-property :raw-link link))))))

  ;; Keymap for Org link actions.
  (defvar-keymap splinter-embark-org-link-map
    :parent embark-general-map
    "RET" 'org-open-at-point
    "l" 'org-insert-link
    "n" 'org-next-link
    "p" 'org-previous-link)

  (add-to-list 'embark-keymap-alist '(org-link . splinter-embark-org-link-map))
  :hook
  (org-mode . (lambda ()
                (make-local-variable 'embark-target-finders)
                (add-to-list 'embark-target-finders 'splinter-embark-org-link))))

(use-package evil-org
  :ensure t
  :after (org general)
  :init
  (defun splinter-evil-org-agenda-set-keys ()
    "Set normal state keys for `org-agenda'. Where
  `org-agenda-set-keys' uses motion state, I use normal state, because
  it works better with my leader key definitions."
    (evil-set-initial-state 'org-agenda-mode 'normal)
    (general-define-key
     :keymaps 'org-agenda-mode-map
     :states 'normal
     "TAB" 'org-agenda-goto
     "g TAB" 'org-agenda-goto
     "RET" 'org-agenda-switch-to
     "M-RET" 'org-agenda-recenter
     "<delete>" 'org-agenda-show-scroll-down
     "<backspace>" 'org-agenda-show-scroll-down
     "j" 'org-agenda-next-line
     "k" 'org-agenda-previous-line
     "gj" 'org-agenda-next-item
     "gk" 'org-agenda-previous-item
     "gH" 'evil-window-top
     "gM" 'evil-window-middle
     "gL" 'evil-window-bottom
     "C-j" 'org-agenda-next-item
     "C-k" 'org-agenda-previous-item
     "[" 'org-agenda-earlier
     "]" 'org-agenda-later
     "J" 'org-agenda-priority-down
     "K" 'org-agenda-priority-up
     "H" 'org-agenda-do-date-earlier
     "L" 'org-agenda-do-date-later
     "t" 'org-agenda-todo
     "M-j" 'org-agenda-drag-line-forward
     "M-k" 'org-agenda-drag-line-backward
     "C-S-h" 'org-agenda-todo-previousset
     "C-S-l" 'org-agenda-todo-nextset
     "u" 'org-agenda-undo
     "dd" 'org-agenda-kill
     "dA" 'org-agenda-archive
     "da" 'org-agenda-archive-default-with-confirmation
     "ct" 'org-agenda-set-tags
     "ce" 'org-agenda-set-effort
     "cT" 'org-timer-set-timer
     "i" 'org-agenda-diary-entry
     "a" 'org-agenda-add-note
     "A" 'org-agenda-append-agenda
     "C" 'org-agenda-capture
     "m" 'org-agenda-bulk-toggle
     "~" 'org-agenda-bulk-toggle-all
     "*" 'org-agenda-bulk-mark-all
     "%" 'org-agenda-bulk-mark-regexp
     "M" 'org-agenda-bulk-remove-all-marks
     "x" 'org-agenda-bulk-action
     "gr" 'org-agenda-redo
     "gR" 'org-agenda-redo-all
     "ZQ" 'org-agenda-exit
     "ZZ" 'org-agenda-quit
     "gD" 'org-agenda-view-mode-dispatch
     "ZD" 'org-agenda-dim-blocked-tasks
     "sc" 'org-agenda-filter-by-category
     "sr" 'org-agenda-filter-by-regexp
     "se" 'org-agenda-filter-by-effort
     "st" 'org-agenda-filter-by-tag
     "s^" 'org-agenda-filter-by-top-headline
     "ss" 'org-agenda-limit-interactively
     "S" 'org-agenda-filter-remove-all
     "I" 'org-agenda-clock-in
     "O" 'org-agenda-clock-out
     "cg" 'org-agenda-clock-goto
     "cc" 'org-agenda-clock-cancel
     "cr" 'org-agenda-clockreport-mode
     "." 'org-agenda-goto-today
     "gc" 'org-agenda-goto-calendar
     "gC" 'org-agenda-convert-date
     "gd" 'org-agenda-goto-date
     "gh" 'org-agenda-holidays
     "gm" 'org-agenda-phases-of-moon
     "gs" 'org-agenda-sunrise-sunset
     "gt" 'org-agenda-show-tags
     "p" 'org-agenda-date-prompt
     "P" 'org-agenda-show-the-flagging-note
     "+" 'org-agenda-manipulate-query-add
     "-" 'org-agenda-manipulate-query-subtract
     "[" 'org-agenda-later
     "]" 'org-agenda-later))
  :hook
  (org-mode . evil-org-mode)
  (org-agenda-mode . splinter-evil-org-agenda-set-keys))

(use-package org-alert
  :disabled
  :ensure t
  :custom
  (alert-default-style 'libnotify)
  (org-alert-notification-title "Agenda"))

(use-package org-fragtog
  :ensure t
  :after org
  :hook
  (org-mode . org-fragtog-mode))

(use-package org-super-agenda
  :ensure t
  :after org
  :hook
  (org-agenda-mode . org-super-agenda-mode)
  :custom
  (org-super-agenda-groups '((:name none
                                    :time-grid t)
                             (:name "Belangrijk"
                                    :priority "A")
                             (:name "Huiswerk"
                                    :tag ("huiswerk" "homework"))
                             (:name "TODO"
                                    :tag "TODO")
                             (:name "Overig"
                                    :anything t))) 
  (org-super-agenda-header-separator "")
  :init
  ;; Disable the org-super-agenda-header-map, because it
  ;; interferes with evil-org.
  (setq org-super-agenda-header-map (make-sparse-keymap)))

(use-package org-roam
  :ensure t
  :after (general)
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y-%m-%d-%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("r" "reference" plain "%?"
      :if-new (file+head "references/${citekey}.org"
                         "#+title: ${title}\n"))))
  (org-roam-ui-latex-macros
   '(("\\id" . "1")
     ("\\setpred" . "\\left\\{\\, #1 \\mid #2 \\,\\right\\}")
     ("\\triv" . "\\operatorname{triv}")
     ("\\dom" . "\\operatorname{dom}")
     ("\\cod" . "\\operatorname{cod}")
     ("\\im" . "\\operatorname{im}")
     ("\\cat" . "\\mathscr{#1}")
     ("\\catSet" . "\\mathbf{Set}")
     ("\\catPointedSet" . "\\mathbf{Set}_*")
     ("\\catCategory" . "\\mathbf{Cat}")
     ("\\catPointedCategory" . "\\mathbf{Set}")
     ("\\catPointedCategory" . "\\mathbf{Cat}_*")
     ("\\catGroupoid" . "\\mathbf{Grpd}")
     ("\\catMonoid" . "\\mathbf{Mon}")
     ("\\catGroup" . "\\mathbf{Grp}")
     ("\\catAbgroup" . "\\mathbf{Ab}")
     ("\\catRing" . "\\mathbf{Ring}")
     ("\\catField" . "\\mathbf{Field}")
     ("\\catGraph" . "\\mathbf{Graph}")
     ("\\catDirectedGraph" . "\\mathbf{Graph}_{\\to}")
     ("\\catTopologicalSpace" . "\\mathbf{Top}")
     ("\\catPointedTopoligicalSpace" . "\\mathbf{Top}_*")
     ("\\catVectorSpace" . "\\mathbf{Vect}")
     ("\\catModule" . "\\mathbf{Mod}")
     ("\\catChainComplex" . "\\mathbf{Ch}")
     ("\\catSimplex" . "\\mathbf{\\Delta}")
     ("\\catSimplicial" . "{\\mathbf{s}#1}")
     ("\\catSimplicialSet" . "\\catSimplicial{\\catSet}")
     ("\\catHomotopy" . "\\operatorname{\\mathbf{Ho}}")
     ("\\deloop" . "\\mathbf{B}")
     ("\\nerve" . "\\deloop")
     ("\\leftadj" . "\\dashv")
     ("\\rightadj" . "\\vdash")
     ("\\lefthomotopic" . "\\sim_L")
     ("\\righthomotopic" . "\\sim_R")
     ("\\leftderived" . "\\mathrm{L}")
     ("\\rightderived" . "\\mathrm{R}")
     ("\\totalleftderived" . "\\mathbf{L}")
     ("\\totalrightderived" . "\\mathbf{R}")
     ("\\weakequivalences" . "\\mathcal{W}")
     ("\\cofibrations" . "\\mathcal{C}")
     ("\\fibrations" . "\\mathcal{F}")
     ("\\colim" . "\\operatorname{colim}")
     ("\\coker" . "\\operatorname{coker}")
     ("\\inj" . "\\operatorname{inj}")
     ("\\pr" . "\\operatorname{pr}")
     ("\\qed" . "\\square")
     ("\\To" . "\\Rightarrow")
     ("\\opp" . "^{\\mathrm{op}}")
     ("\\bb" . "\\mathbb{#1}")
     ("\\Hom" . "\\operatorname{Hom}")
     ("\\End" . "\\operatorname{End}")
     ("\\Aut" . "\\operatorname{Aut}")
     ("\\Sing" . "\\operatorname{Sing}")))
  :config
  (org-roam-db-autosync-mode)
  :general
  (my-leader-def
    "nc" '(org-roam-capture :which-key "Capture")
    "nf" '(org-roam-node-find :which-key "Find node")
    "ni" '(org-roam-node-insert :which-key "Insert node")
    "nl" '(org-roam-buffer-toggle :which-key "Toggle org-roam buffer")
    "nr" '(orb-insert-link :which-key "Cite")))

(use-package org-roam-ui
  :ensure t
  :after (org-roam)
  :custom
  (org-roam-ui-sync-theme nil)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package org-ref
  :ensure t
  :custom
  (bibtex-completion-bibliography '("~/Zotero/biblatex.bib"))
  (bibtex-compeltion-notes-path "~/Documents/notes/references/")
  (bibtex-completion-pdf-field "file"))

(use-package org-roam-bibtex
  :ensure t
  :after (org-roam org-ref)
  :hook (org-roam-mode . org-roam-bibtex-mode))

(use-package org-roam-dailies
  :disabled
  :ensure org-roam
  :after (general)
  :custom
  (org-roam-dailies-directory "dag/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :if-new (file+head "%<%Y-%m-%d>.org.gpg"
                         "#+title: %<%Y-%m-%d>\n"))))
  :general
  (my-leader-def
    "nd" '(org-roam-dailies-capture-today :which-key "Capture daily")))

(use-package olivetti
  :ensure t)

(provide 'init-org-mode)
