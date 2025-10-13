;;; -*- lexical-binding: t -*-

(require 'init-git)
(require 'init-keybindings)
(require 'init-modeline)

(defvar after-load-theme-hook nil
  "Hook that is run after ‘load-theme’ is called.")

(defun load-theme--after-advice (_oldfun &rest _args)
  "Run ‘after-load-theme-hook’."
  (run-hooks 'after-load-theme-hook))
(advice-add 'load-theme :after 'load-theme--after-advice)

(defun splinter-load-theme (theme)
  "Disable all custom enabled themes and then load THEME."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (mapc 'disable-theme custom-enabled-themes)
  (load-theme theme t))

(defvar splinter-themes nil
  "A list of functions that enable themes, that can be cycled
  through with ‘SPLINTER-SWITCH-THEME’.")

(defun splinter-switch-theme ()
  "Switch to the next theme in ‘SPLINTER-THEMES’."
  (interactive)
  (when-let ((next (pop splinter-themes)))
    (setq splinter-themes (append splinter-themes (list next)))
    (funcall next)))

(use-package base16-theme
  :disabled
  :ensure t
  :demand t
  :after evil
  :custom
  (base16-distinct-fringe-background nil)
  :config
  (defun splinter-load-base16-theme (theme)
    "Load a base-16 theme. This function is used instead of
  just ‘(load-theme theme t)’ to also set the colours of the evil
  cursors."
    ;; Set evil cursors.
    (let ((colors (intern (concat (symbol-name theme) "-colors"))))
      (setq evil-emacs-state-cursor   `(,(plist-get colors :base0D) box)
            evil-insert-state-cursor  `(,(plist-get colors :base0D) bar)
            evil-motion-state-cursor  `(,(plist-get colors :base0E) box)
            evil-normal-state-cursor  `(,(plist-get colors :base0B) box)
            evil-replace-state-cursor `(,(plist-get colors :base08) bar)
            evil-visual-state-cursor  `(,(plist-get colors :base09) box)))
    (splinter-load-theme theme))

  (add-to-list 'splinter-themes
               (lambda ()
                 (splinter-load-base16-theme 'base16-tomorrow)))
  (add-to-list 'splinter-themes
               (lambda ()
                 (splinter-load-base16-theme 'base16-tomorrow-night)))

  (splinter-load-base16-theme 'base16-tomorrow-night))

(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-completions '((matches . (background intense))))
  (modus-themes-headings '((1 . (rainbow background overline variable-pitch 1.3))
                           (2 . (rainbow background overline variable-pitch 1.2))
                           (3 . (rainbow background overline variable-pitch 1.1))
                           (t . (rainbow background overline variable-pitch))))
  (modus-themes-links '(background))
  (modus-themes-paren-match '(bold intense))
  (modus-themes-region '(bg-only accented))
  :config
  (defun splinter-load-modus-theme (theme)
    "Load a Modus theme.

This function loads a Modus theme and configures some faces to my
liking."
    (interactive
     (list
      (intern (completing-read "Load Modus theme: " '(modus-vivendi modus-operandi)))))
    (splinter-load-theme theme)
    ;; Set the foreground of the git-gutter-fringe faces to the colour that is
    ;; normally used for their background.
    (modus-themes-with-colors
      (set-face-attribute 'git-gutter-fr:added nil
                          :background bg-dim
                          :foreground green-cooler)
      (set-face-attribute 'git-gutter-fr:deleted nil
                          :background bg-dim
                          :foreground red-cooler)
      (set-face-attribute 'git-gutter-fr:modified nil
                          :background bg-dim
                          :foreground yellow-intense))
    ;; Hide mode line bar.
    (set-face-attribute 'doom-modeline-bar nil
                        :background 'unspecified
                        :inherit 'mode-line)
    (set-face-attribute 'doom-modeline-bar-inactive nil
                        :background 'unspecified
                        :inherit 'mode-line-inactive))
  (add-to-list 'splinter-themes
               (lambda ()
                 (splinter-load-modus-theme 'modus-vivendi)))
  (add-to-list 'splinter-themes
               (lambda ()
                 (splinter-load-modus-theme 'modus-operandi))))

(use-package ef-themes
  :ensure t
  :config
  (defun splinter-ef-themes-color (theme color)
    "Return color value for COLOR from palette of Ef theme THEME."
    (ef-themes-get-color-value color nil theme))
  (defun splinter-load-ef-theme (theme)
    "Load an Ef theme.

This function loads an Ef theme and configures some faces to my
liking."
    (interactive
     (list
      (intern (completing-read "Load Ef theme: "
                               (mapcar 'symbol-name ef-themes-collection)))))
    (splinter-load-theme theme)
    (ef-themes-with-colors
      ;; Set the foreground of the git-gutter-fringe faces to the colour that is
      ;; normally used for their background.
      (set-face-attribute 'git-gutter-fr:added nil
                          :background bg-main
                          :foreground green-warmer)
      (set-face-attribute 'git-gutter-fr:deleted nil
                          :background bg-main
                          :foreground red-warmer)
      (set-face-attribute 'git-gutter-fr:modified nil
                          :background bg-main
                          :foreground yellow-warmer)
      ;; Hide mode line bar.
      (set-face-attribute 'doom-modeline-bar nil
                          :background 'unspecified
                          :inherit 'mode-line)
      (set-face-attribute 'doom-modeline-bar-inactive nil
                          :background 'unspecified
                          :inherit 'mode-line-inactive)
      ;; Set org-roam-ui colours.
      ;; (setq org-roam-ui-custom-theme
      ;;       `((bg . ,bg-main)
      ;;         (bg-alt . ,bg-dim)
      ;;         (fg . ,fg-dim)
      ;;         (fg-alt . ,fg-dim)
      ;;         (red . ,red)
      ;;         (orange . ,red)
      ;;         (yellow . ,yellow)
      ;;         (green . ,green)
      ;;         (cyan . ,cyan)
      ;;         (blue . ,blue)
      ;;         (violet . ,magenta-cooler)
      ;;         (magenta . ,magenta)))
      ))
  ;; (add-to-list 'splinter-themes
  ;;              (lambda ()
  ;;                (splinter-load-ef-theme 'ef-dark)))
  ;; (add-to-list 'splinter-themes
  ;;              (lambda ()
  ;;                (splinter-load-ef-theme 'ef-light)))
  )


(use-package flexoki-themes
  :demand t
  :init)

(use-package cosmic-theme-watcher
  :demand t
  :after (flexoki-themes)
  :custom
  (cosmic-theme-watcher-light-theme 'flexoki-light)
  (cosmic-theme-watcher-dark-theme 'flexoki-dark)
  (cosmic-theme-watcher-load-theme-function 'splinter-load-theme)
  :config
  (cosmic-theme-watcher-enable))

(use-package custom
  :demand t
  :after (cosmic-theme-watcher)
  :general
  (my-leader-def
    "yl" '(splinter-load-theme :which-key "Load theme")
    "yy" '(cosmic-theme-watcher-toggle :which-key "Switch theme")))

(provide 'init-theme)
