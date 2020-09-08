{ config, ... }:
let
  documents = config.xdg.userDirs.documents;
in 
{
  programs.emacs.init.usePackage = {
    org = {
      enable = true;
      hook = [ "(org-mode . auto-fill-mode) "];
      after = [ "evil-leader" ];
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

            (evil-leader/set-key
              "oa" 'org-agenda
              "on" '(lambda ()
	                    (interactive)
	                    (org-agenda nil "n"))
              "oo" '(lambda ()
	                    (interactive)
	                    (org-agenda nil "n")
                      (delete-other-windows)))
      '';
      config = ''
        (add-to-list 'org-modules 'org-tempo)
        (add-to-list 'org-modules 'org-habit)

        (evil-leader/set-key-for-mode 'org-mode
          "c!" 'org-time-stamp-inactive
          "c#" 'org-update-statistics-cookies
          "c$" 'org-archive-subtree
          "c%" 'org-mark-ring-push
          "c'" 'org-edit-special
          "c*" 'org-ctrl-c-star
          "c+" 'org-table-sum
          "c," 'org-priority
          "c-" 'org-ctrl-c-minus
          "c." 'org-time-stamp
          "c/" 'org-sparse-tree
          "c:" 'org-toggle-fixed-width
          "c;" 'org-toggle-comment
          "c<" 'org-date-from-calendar
          "c=" 'org-table-eval-formula
          "c>" 'org-goto-calendar
          "c?" 'org-table-field-info
          "c@" 'org-mark-subtree
          "c[" 'org-agenda-file-to-front
          "c\\" 'org-match-sparse-tree
          "c]" 'org-remove-file
          "c^" 'org-sort
          "c`" 'org-table-edit-field
          "c{" 'org-table-toggle-formula-debugger
          "c|" 'org-table-create-or-convert-from-region
          "c}" 'org-table-toggle-coordinate-overlays
          "c~" 'org-table-create-with-table.el
          ;; "c*" 'org-list-make-subtree
          ;; "c," 'org-insert-structure-template
          ;; "c<" 'outline-promote
          ;; "c>" 'outline-demote
          ;; "c^" 'org-up-element
          ;; "c_" 'org-down-element
          "ca" 'org-attach
          "cb" 'org-backward-heading-same-level
          "cc" 'org-ctrl-c-ctrl-c
          "cd" 'org-deadline
          "ce" 'org-export-dispatch
          "cf" 'org-forward-heading-same-level
          "cj" 'org-goto
          "ck" 'org-kill-note-or-show-branches
          "cl" 'org-insert-link
          "cn" 'outline-next-visible-heading
          "co" 'org-open-at-point
          "cp" 'outline-previous-visible-heading
          "cq" 'org-set-tags-command
          "cr" 'org-reveal
          "cs" 'org-schedule
          "ct" 'org-todo
          "cu" 'outline-up-heading
          ;; "cv" TODO
          "cw" 'org-refile
          "cx!" 'org-reload
          "cx," 'org-timer-pause-or-continue
          "cx-" 'org-timer-item
          "cx." 'org-timer
          "cx0" 'org-timer-start
          "cx;" 'org-timer-set-timer
          "cx<" 'org-agenda-set-restriction-lock
          "cx>" 'org-agenda-remove-restriction-lock
          "cxA" 'org-archive-to-archive-sibling
          "cxE" 'org-inc-effort
          "cxG" 'org-feed-goto-inbox
          "cxI" 'org-info-find-node
          "cxP" 'org-set-property-and-value
          "cx[" 'org-reftex-citation
          "cx\\" 'org-toggle-pretty-entities
          "cx_" 'org-timer-stop
          "cxa" 'org-toggle-archive-tag
          "cxb" 'org-tree-to-indirect-buffer
          "cxc" 'org-clone-subtree-with-time-shift
          "cxd" 'org-insert-drawer
          "cxe" 'org-set-effort
          "cxf" 'org-footnote-action
          "cxg" 'org-feed-update-all
          "cxi" 'org-columns-insert-dblock
          "cxo" 'org-toggle-ordered-property
          "cxp" 'org-set-property
          "cxq" 'org-toggle-tags-groups
          "cxv" 'org-copy-visible
          "cx TAB" 'org-clock-in
          "cxA" 'org-archive-subtree
          "cxB" 'org-toggle-checkbox
          "cxC" 'org-columns
          "cxD" 'org-clock-display
          "cxE" 'org-clock-modify-effort-estimate
          "cxF" 'org-emphasize
          "cxJ" 'org-clock-goto
          "cxL" 'org-toggle-latex-fragment
          "cxN" 'org-next-link
          "cxO" 'org-clock-out
          "cxP" 'org-previous-link
          "cxQ" 'org-clock-cancel
          "cxR" 'org-clock-report
          "cxS" 'org-archive-subtree
          "cxT" 'org-toggle-time-stamp-overlays
          "cxU" 'org-dblock-update
          "cxV" 'org-toggle-inline-images
          "cxW" 'org-cut-special
          "cy" 'org-evaluate-time-range)

          (evil-leader/set-key-for-mode 'org-agenda-mode
            "c," 'org-agenda-priority
            "ca" 'org-attach
            "cd" 'org-agenda-deadline
            "cn" 'org-agenda-next-date-line
            "co" 'org-agenda-open-link
            "cp" 'org-agenda-next-date-line
            "cq" 'org-agenda-set-tags
            "cs" 'org-agenda-schedule
            "ct" 'org-agenda-todo
            "cw" 'org-agenda-refile
            ;; TODO: add more keys under cx.
            "cx TAB" 'org-agenda-clock-in
            "cxo" 'org-agenda-clock-out
            "cz" 'org-agenda-add-note)

        ;; TODO: does not seem to work.
        (evil-leader/set-key-for-mode 'org-src-mode
          "c'" 'org-edit-src-exit
          "ck" 'org-edit-src-abort)
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

    org-ref = {
      enable = false;
    };
  };
}
