;; Packages are retrieved using Nix, not directly from Elpa or Melpa.
(require 'package)
(package-initialize)
(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))

(load-directory "~/.emacs.d/elisp")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2f846ff1efa08005b60943a1710a48c401630c36a4dc48339353c37e935eac18" "290fdd9d3620344db9008842719df73bbd1d7fd5b0c8a0c1dbe5e2e27473f673" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "332fcf3c7208aca9fab65d54203f78a242482e7fd65f5725a2482c20b1730732" default)))
 '(package-selected-packages
   (quote
    (zettelkasten agda2-mode org-ref pdf-tools evil-collection nix-company irony nix hs-minor hs-minor-mode yasnippet yaml-mode which-key use-package tuareg switch-window spacemacs-theme spaceline slime sane-term rust-mode org-evil nix-mode multiple-cursors magit-todos idris-mode ido-vertical-mode htmlize hindent highlight-numbers gruvbox-theme git-gutter flycheck fill-column-indicator expand-region evil-surround evil-numbers evil-leader auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-datatype-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-field-face ((t (:inherit font-lock-function-name-face))))
 '(agda2-highlight-function-face ((t (:inherit font-lock-function-name-face))))
 '(agda2-highlight-inductive-constructor-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-keyword-face ((t (:inherit font-lock-keyword-face))))
 '(agda2-highlight-module-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-number-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-operator-face ((t (:inherit font-lock-variable-name-face))))
 '(agda2-highlight-postulate-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-primitive-face ((t (:inherit font-lock-function-name-face))))
 '(agda2-highlight-primitive-type-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-record-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-symbol-face ((t (:inherit font-lock-variable-name-face)))))
