{ pkgs, ... }: {
  programs.emacs.init.usePackage = {
    doom-modeline = {
      enable = true;
      init = ''
        (setq doom-modeline-height 22)
        (setq doom-modeline-bar-width 2)
        (setq doom-modeline-mu4e t)
        (setq doom-modeline-icon t)
      '';
      hook = [ "(after-init . doom-modeline-mode)" ];
    };

    feebleline = {
      enable = false;
      after = [ "evil" ];
      config = ''
        ;; Make the tag " <V> " instead of 'evil-visual-tag for the
        ;; visual state.
        (setq evil-visual-state-tag " <V> ")
        (defun my/feebleline-evil-state ()
          "Return current evil state."
          (evil-state-property evil-state :tag t))

        (defun my/feebleline-major-mode ()
          "Return current major mode."
          (or mode-name ""))

        (defun my/feebleline-minor-modes ()
          "Return current minor modes."
          (string-join (split-string (format-mode-line minor-mode-alist)) "|"))

        (setq feebleline-msg-functions
              '((my/feebleline-evil-state       :face bold)
                (feebleline-file-directory      :face feebleline-dir-face :post "")
                (feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
                (feebleline-file-modified-star  :face font-lock-warning-face :post "")
                (feebleline-git-branch          :face feebleline-git-face :pre " ")
                (my/feebleline-major-mode)
                ;; (my/feebleline-minor-modes      :face font-lock-comment-face)
                (feebleline-project-name        :align right)
                (feebleline-line-number         :align right :post "" :fmt "%s")
                (feebleline-column-number       :align right :pre ":" :fmt "%s")))

        (feebleline-mode)
      '';
    };
  };
}
