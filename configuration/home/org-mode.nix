{ pkgs, config, ... }:
let documents = config.xdg.userDirs.documents;
in {
  programs.emacs.init.usePackage = {
    org = {
      enable = true;
      hook = [ "(org-mode . auto-fill-mode)" ];
      after = [ "general" ];
      init = ''
        (setq org-log-done 'time)
        (setq org-catch-invisible-edits 'show-and-error)
        (setq org-list-indent-offset 0)
        (setq org-list-demote-modify-bullet
              '(("+" . "-") ("-" . "+") ("*" . "+")))
        (setq org-startup-indented nil)
        (setq sentence-end-double-space nil)
        (setq org-hide-emphasis-markers t)
        (setq org-highlight-latex-and-related '(latex script))

        (setq org-agenda-files '("${documents}/agenda/"))
        (setq calendar-week-start-day 1)
        (setq calendar-day-name-array ["zondag" "maandag" "dinsdag" "woensdag"
                                       "donderdag" "vrijdag" "zaterdag"])
        (setq calendar-month-name-array ["januari" "februari" "maart" "april" "mei"
                                         "juni" "juli" "augustus" "september"
                                         "oktober" "november" "december"])

        (general-define-key
          :prefix my-leader
          :states '(normal visual motion)
          :keymaps 'override
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
      '';
      config = ''
        (add-to-list 'org-modules 'org-tempo)
        (add-to-list 'org-modules 'org-habit)

        (general-define-key
          :prefix my-local-leader
          :states '(normal visual motion)
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

        ;; TODO: doesn't seem to work.
        ;; Unbind leader key.
        (general-define-key
          :prefix my-leader
          :states '(normal visual motion)
          :keymaps 'org-agenda-mode-map
          "" nil)
        (general-define-key
          :prefix my-local-leader
          :states '(normal visual motion)
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
        (general-define-key
          :prefix my-local-leader
          :states '(normal visual motion)
          :keymaps 'org-src-mode-map
          "'" 'org-edit-src-exit
          "k" 'org-edit-src-abort)
      '';
    };

    evil-org = {
      enable = true;
      after = [ "org" ];
      hook = [
        "(org-mode . evil-org-mode)"
        ''
          (org-agenda-mode . (lambda ()
                               (require 'evil-org-agenda)
                               (evil-org-agenda-set-keys)))
        ''
      ];
    };

    org-alert = {
      enable = false;
      init = ''
        (setq alert-default-style 'libnotify)
        (setq org-alert-notification-title "Agenda")
      '';
    };

    org-ref = { enable = false; };

    org-fragtog = {
      enable = true;
      after = [ "org" ];
      hook = [ "(org-mode . org-fragtog-mode)" ];
    };

    org-super-agenda = {
      enable = true;
      after = [ "org" ];
      hook = [ "(org-agenda-mode . org-super-agenda-mode)" ];
      init = ''
        (setq org-super-agenda-groups
              '((:name none
                       :time-grid t)
                (:name "Belangrijk"
                       :priority "A")
                (:name "Huiswerk"
                       :tag ("huiswerk" "homework"))
                (:name "TODO"
                       :tag "TODO")
                (:name "Overig"
                       :anything t))) 

        (setq org-super-agenda-header-separator "")

        ;; Disable the org-super-agenda-header-map, because it
        ;; interferes with evil-org.
        (setq org-super-agenda-header-map (make-sparse-keymap))
      '';
    };

    org-roam = {
      enable = true;
      after = [ "general" ];
      init = ''
        (setq org-roam-directory "${documents}/notities")

        ;; NOTE: not setting this causes an error, because
        ;;   (boundp 'emacsql-sqlite3-executable)
        ;;   => t
        ;; but
        ;;   emacsql-sqlite3-executable
        ;;   => nil
        ;; which makes
        ;;   (file-executable-p emacsql-sqlite3-executable)
        ;;   => error
        (setq emacsql-sqlite3-executable "${pkgs.sqlite}/bin/sqlite3")

        (setq org-roam-graph-executable "${pkgs.graphviz}/bin/dot")

        (setq org-roam-capture-templates
              '(("d" "default" plain (function org-roam--capture-get-point)
                 "%?"
                 :file-name "%<%Y-%m-%d-%H%M> ''${title}"
                 :head "#+title: ''${title}\n"
                 :unnarrowed t)))
      '';
      config = ''
        (general-define-key
          :prefix my-leader
          :states '(normal visual motion)
          :keymaps 'override
          "zf" '(org-roam-find-file :which-key "Find file")
          "zi" '(org-roam-insert :which-key "Insert")
          "zr" '(org-roam :which-key "Toggle org-roam buffer"))
      '';
    };
  };
}
