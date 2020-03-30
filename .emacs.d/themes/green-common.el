(provide 'green-common)

(defun create-green-theme (dark theme-name)
  (let ((bg0 (if dark "#000000" "#eeeeee"))
        (bg1 (if dark "#111111" "#dddddd"))
        (bg2 (if dark "#4b4b4b" "#787878"))
        (fg0 (if dark "#787878" "#000000"))
        (fg1 (if dark "#a5a5a5" "#111111"))
        (fg2 (if dark "#dddddd" "#4b4b4b"))
        ;; (black "#282a2e")
        (black "#373a42")
        (grey "#4b4b4b")
        (red "#ec6363")
        ;; (green "#24cd39")
        ;; (green "#2bb63d")
        (green "#4adb5c")
        (yellow "#a6c35e")
        (blue "#4697db")
        (purple "#b883d8")
        (cyan "#4eb6a9")
        (white "#a5a5a5"))

    (custom-theme-set-faces theme-name
     `(default ((t (:foreground ,fg0 :background ,bg0))))
     `(cursor ((t (:background ,fg0))))
     `(fringe ((t (:background ,bg0))))
     `(highlight ((t (:foreground ,fg1 :background ,bg1))))
     `(hl-line ((t (:background ,bg1))))
     `(region ((t (:background ,bg2))))
     `(secondary-selection ((t (:background ,green))))
     `(isearch ((t (:background ,green :foreground ,bg0))))
     `(lazy-highlight ((t (:background ,yellow :foreground ,bg0))))
     `(line-number ((t (:background ,bg0 :foreground ,fg0))))
     `(line-number-current-line ((t (:background ,bg0 :foreground ,fg0))))
     `(linum ((t (:background ,bg0 :foreground ,fg0))))
     `(mode-line ((t (:box (:color ,green :line-width 1))
                     :background ,bg0
                     :foreground ,fg1)))
     `(mode-line-inactive ((t (:box (:color ,green :line-width 1))
                              :background ,bg0
                              :foreground ,fg0)))
     `(minibuffer-prompt ((t (:foreground ,fg1 :weight bold))))
     `(escape-glyph ((t (:foreground ,green))))
     `(homoglyph ((t (:foreground ,green))))
     `(error ((t (:foreground ,red))))
     `(warning ((t (:foreground ,yellow))))
     `(success ((t (:foreground ,green))))
     `(font-lock-builtin-face ((t (:foreground ,green :weight bold))))
     `(font-lock-comment-face ((t (:foreground ,(if dark fg0 fg2) :slant italic))))
     `(font-lock-constant-face ((t (:foreground ,green :weight bold))))
     `(font-lock-function-name-face ((t (:foreground ,fg1 :weight bold))))
     `(font-lock-keyword-face ((t (:foreground ,fg1 :weight bold))))
     `(font-lock-string-face ((t (:foreground ,green))))
     `(font-lock-type-face ((t (:foreground ,fg0 :weight bold))))
     `(font-lock-variable-name-face ((t (:foreground ,fg1 :weight bold))))
     `(link ((t (:foreground ,blue :underline t))))
     `(link-visited ((t (:foreground ,purple :underline t))))
     
     ;; Paren showing faces
     `(show-paren-match ((t (:foreground ,green :background ,bg2))))
     `(show-paren-mismatch ((t (:foreground ,red :background ,bg2))))
     
     `(company-tooltip ((t (:foreground ,fg0 :background ,bg1))))
     `(company-tooltip-selection ((t (:foreground ,fg1 :background ,bg2))))
     `(company-tooltip-common ((t (:foreground ,green :background ,bg1))))
     `(company-tooltip-common-selection ((t (:foreground ,green :background ,bg2))))
     `(company-scrollbar-bg ((t (:background ,bg1))))
     `(company-scrollbar-fg ((t (:foreground ,fg0 :background ,fg0))))
     
     ;; Ido faces
     `(ido-subdir ((t (:foreground ,fg1 :weight bold))))
     `(ido-only-match ((t (:foreground ,green :weight bold))))
     
     ;; Widget faces
     `(widget-field ((t (:background ,bg2 :foreground ,fg1))))
     `(widget-button-pressed ((t (:foreground ,red))))
     
     ;; Agda faces
     `(agda2-highlight-inductive-constructor-face ((t (:foreground ,green))))
     ;; TODO: add more.
     
     ;; Org mode faces
     `(org-level-1 ((t (:foreground ,green :weight bold))))
     `(org-level-2 ((t (:foreground ,blue :weight bold))))
     `(org-level-3 ((t (:foreground ,yellow :weight bold))))
     `(org-level-4 ((t (:foreground ,fg1))))
     `(org-level-5 ((t (:foreground ,fg1))))
     `(org-done ((t (:foreground ,green :weight bold))))
     `(org-todo ((t (:foreground ,red :weight bold))))
     `(org-date ((t (:foreground ,bg2 :underline t))))
     `(org-agenda-date ((t (:foreground ,green))))
     `(org-agenda-structure ((t (:foreground ,fg1 :weight bold))))
     
     ;; eshell faces
     `(eshell-prompt ((t (:foreground ,green :weight bold))))
     
     ;; LaTeX faces
     `(font-latex-sectioning-5-face ((t (:foreground ,green :weight bold))))
     `(font-latex-italic-face ((t (:inherit italic :foreground ,fg1))))
     `(font-latex-warning-face ((t (:inherit warning))))
     )

    (custom-theme-set-variables theme-name
     ;; pdf-tools faces
     `(pdf-view-midnight-colors '(,fg0 . ,bg1))
     )))

