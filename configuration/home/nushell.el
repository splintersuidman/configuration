;;; -*- lexical-binding: t -*-

(require 'init-completion)
(require 'init-keybindings)
(require 'init-vterm)

(use-package reformatter
  :ensure t)

(use-package nushell-mode
  :ensure t
  :mode ("\\.nu\\'" . nushell-mode)
  :after (general vterm)
  :custom
  (vterm-shell "nu")
  :config
  (reformatter-define nushell-format
    :program "nufmt"
    :args '("--stdin")
    :group 'nushell-mode
    :lighter " Nufmt")
  ;;;###autoload (autoload 'nushell-format-buffer "nushell-mode" nil t)
  ;;;###autoload (autoload 'nushell-format-region "nushell-mode" nil t)
  ;;;###autoload (autoload 'nushell-format-on-save-mode "nushell-mode" nil t)
  (defun splinter-consult-vterm-nu-history ()
    "Select command from Nu history in Vterm."
    (interactive)
    (let* ((history (with-temp-buffer
                      (insert-file-contents-literally "~/.config/nushell/history.txt")
                      (nreverse (string-lines (buffer-string)))))
           (line (consult--read history
                                :prompt "Select: "
                                :sort nil)))
      (when line
        ;; NOTE: This is not really correct: it does not delete multiline
        ;; input correctly. It does suffice in many cases, though.
        (vterm-send "C-e")
        (vterm-send "C-u")
        (vterm-send-string line))))
  :general
  (:keymaps 'vterm-mode-map
   :states '(normal insert visual)
   "C-r" '(splinter-consult-vterm-nu-history :which-key "History"))
  (my-local-leader-def
    :keymaps 'nushell-mode-map
    "f" '(nushell-format-buffer :which-key "Format buffer")
    "F" '(nushell-format-region :which-key "Format region")))

(provide 'init-nushell)
