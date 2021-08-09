(require 'init-keybindings)

(use-package notmuch
  :ensure t
  :after general
  :commands (notmuch)
  :custom
  (notmuch-show-logo nil)
  (notmuch-hello-auto-refresh t)
  (notmuch-search-oldest-first nil "Show newest first")
  (notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
                            (:name "week" :query "tag:inbox and date:week.." :key "w")
                            (:name "unread" :query "tag:unread" :key "u")
                            (:name "flagged" :query "tag:flagged" :key "f")
                            (:name "sent" :query "tag:sent" :key "t")
                            (:name "drafts" :query "tag:draft" :key "d")
                            (:name "all mail" :query "*" :key "a")))
  :general
  (my-local-leader-def
    :keymaps 'notmuch-common-keymap
    "R" 'notmuch-refresh-all-buffers
    "c" 'notmuch-mua-send-and-exit
    "d" 'notmuch-draft-save
    "p" 'notmuch-draft-postpone
    "r" 'notmuch-refresh-this-buffer
    "s" 'notmuch-mua-send))

(use-package notmuch-notify
  :after (notmuch)
  :custom
  (notmuch-notify-command (lambda (msg)
                            `("notify-send" ,(plist-get msg :authors) ,(plist-get msg :subject)))))

(use-package notmuch-update
  :after (general notmuch notmuch-notify)
  :functions (notmuch-update-add-hook)
  :custom
  (notmuch-update-command "mbsync --all && notmuch new")
  (notmuch-update-interval (* 5 60))
  :init
  (defun splinter-notmuch-refresh-non-show-buffers ()
    "Invoke `notmuch-refresh-this-buffer' on all notmuch major-mode buffers
except for `notmuch-show-mode' buffers."
    (interactive)
    (dolist (buffer (buffer-list))
      (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
        (when (memq buffer-mode '(notmuch-tree-mode notmuch-search-mode notmuch-hello-mode))
          (with-current-buffer buffer
            (notmuch-refresh-this-buffer))))))
  :config
  (notmuch-update-add-hook)
  :hook
  (notmuch-update-post . notmuch-notify)
  (notmuch-update-post . splinter-notmuch-refresh-non-show-buffers)
  :general
  (my-local-leader-def
    :keymaps 'notmuch-common-keymap
    "u" 'notmuch-update))

(use-package message
  :after general
  :general
  (my-local-leader-def
    :keymaps 'message-mode-map
    "a" 'mml-attach-file
    "b" 'message-goto-body
    "c" 'message-send-and-exit
    "d" 'message-dont-send
    "e" 'message-elide-region
    "fa" 'message-generate-unsubscribed-mail-followup-to
    "fb" 'message-goto-bcc
    "fc" 'message-goto-cc
    "fd" 'message-goto-distribution
    "fe" 'message-insert-expires
    "ff" 'message-goto-followup-to
    "f TAB" 'message-insert-or-toggle-importance
    "fk" 'message-goto-keywords
    "f RET" 'message-goto-mail-followup-to
    "fn" 'message-goto-newsgroups
    "fo" 'message-goto-from
    "fr" 'message-goto-reply-to
    "fs" 'message-goto-subject
    "ft" 'message-goto-to
    "fu" 'message-goto-summary
    "fw" 'message-goto-fcc
    "TAB" 'message-goto-signature
    "k" 'message-kill-buffer
    "l" 'message-to-list-only
    "n" 'message-insert-newsgroups
    "o" 'message-sort-headers
    "q" 'message-fill-yanked-message
    "r" 'message-caesar-buffer-body
    "s" 'message-send
    "t" 'message-insert-to
    "u" 'message-insert-or-toggle-importance
    "v" 'message-delete-not-region
    "w" 'message-insert-signature))

(use-package loaddefs
  :custom
  (send-mail-function 'sendmail-send-it)
  (sendmail-program "msmtp"))

(provide 'init-mail)
