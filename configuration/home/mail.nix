{ pkgs, config, inputs, ... }:
let
  mbsync = "${config.programs.mbsync.package}/bin/mbsync";
  msmtp = "${pkgs.msmtp}/bin/msmtp";
  mu = "${pkgs.mu}/bin/mu";
  mu4eLoadPath = "${pkgs.mu}/share/emacs/site-lisp/mu4e";
  notify-send = "${pkgs.libnotify}/bin/notify-send";
  notmuch = "${pkgs.notmuch}/bin/notmuch";
in {
  programs.mbsync.enable = true;
  programs.mu.enable = false;
  programs.msmtp.enable = true;
  programs.notmuch = {
    enable = true;
    new.tags = [ "unread" "inbox" "notification" ];
  };

  programs.emacs.init.usePackage = {
    notmuch = {
      enable = true;
      after = [ "general" ];
      config = ''
        (general-define-key
          :prefix my-local-leader
          :states '(normal visual motion)
          :keymaps 'notmuch-common-keymap
          "r" 'notmuch-refresh-this-buffer
          "R" 'notmuch-refresh-all-buffers)
      '';
      extraConfig = ''
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

      '';
    };

    consult-notmuch = { enable = true; };

    notmuch-notify = {
      enable = true;
      package = epkgs:
        epkgs.trivialBuild {
          pname = "notmuch-notify";
          src = ./mail/notmuch-notify;
        };
      init = ''
        (setq notmuch-notify-command
              (lambda (msg)
                `("${notify-send}" ,(plist-get msg :authors) ,(plist-get msg :subject))))
      '';
    };

    notmuch-update = {
      enable = true;
      after = [ "general" "notmuch" "notmuch-notify" ];
      package = epkgs:
        epkgs.trivialBuild {
          pname = "notmuch-update";
          src = ./mail/notmuch-update;
          buildInputs = [ epkgs.notmuch ];
        };
      init = ''
        (setq notmuch-update-command "${mbsync} --all && ${notmuch} new")
        (setq notmuch-update-interval (* 5 60))
      '';
      config = ''
        (add-hook 'notmuch-update-post-hook 'notmuch-notify)
        (add-hook 'notmuch-update-post-hook 'notmuch-refresh-all-buffers)
        (notmuch-update-add-hook)

        (general-define-key
         :prefix my-local-leader
         :states '(normal visual motion)
         :keymaps 'notmuch-common-keymap
         "u" 'notmuch-update)

        (general-define-key
         :prefix my-local-leader
         :states '(normal visual motion)
         :keymaps 'notmuch-message-mode-map
         "c" 'notmuch-mua-send-and-exit
         "d" 'notmuch-draft-save
         "s" 'notmuch-mua-send
         "p" 'notmuch-draft-postpone)
      '';
    };

    message = {
      enable = true;
      after = [ "general" ];
      config = ''
       (general-define-key
        :prefix my-local-leader
        :states '(normal visual motion)
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
        "w" 'message-insert-signature) 
      '';
    };

    sendmail = {
      enable = true;
      init = ''
        (setq send-mail-function 'sendmail-send-it)
        (setq sendmail-program "${msmtp}")
      '';
    };

    mu4e = {
      enable = false;
      after = [ "general" ];
      init = ''
        (setq mu4e-mu-binary "${mu}"

              mu4e-get-mail-command "${mbsync} --all"
              mu4e-update-interval (* 60 5)

              mu4e-sent-folder   "/sent"
              mu4e-drafts-folder "/drafts"
              mu4e-trash-folder  "/trash"
              mu4e-refile-folder "/archive"

              mu4e-completing-read-function 'completing-read)

        ;; TODO: configure SMTP from Emacs.
        (setq send-mail-function 'mailclient-send-it)
      '';
      config = ''
        (general-define-key
          :prefix my-local-leader
          :states '(normal visual motion)
          :keymaps 'mu4e-headers-mode-map
          "u" 'mu4e-update-mail-and-index)
      '';
      extraConfig = ''
        :load-path "${mu4eLoadPath}"
      '';
    };

    mu4e-alert = {
      enable = false;
      after = [ "mu4e" ];
      init = ''
        (setq mu4e-alert-email-notification-types '(subjects))
      '';
      config = ''
        (mu4e-alert-set-default-style 'libnotify)
      '';
      hook = [ "(mu4e-main-mode . mu4e-alert-enable-notifications)" ];
    };

    mu4e-views = {
      enable = false;
      after = [ "mu4e" "general" ];
      init = ''
        (setq mu4e-views-completion-method 'default)
      '';
      config = ''
        ;; NOTE: HTML via xwidgets is very buggy, so disable it. (I do
        ;; use the "browser" command, so this package is still useful.)
        (setq mu4e-views-view-commands
              (assoc-delete-all "html" mu4e-views-view-commands))

        (general-define-key
          :prefix my-local-leader
          :states '(normal visual motion)
          :keymaps 'mu4e-headers-mode-map
          "v" 'mu4e-views-mu4e-select-view-msg-method)
      '';
    };

    mu4e-thread-folding = {
      enable = false;
      after = [ "general" "mu4e" ];
      init = ''
        (setq mu4e-thread-folding-root-unfolded-prefix-string "▼")
        (setq mu4e-thread-folding-root-folded-prefix-string "►")

        (defun splinter-mu4e-thread-folding-mode-set-faces (&optional force)
          "Set mu4e-thread-folding faces."
          (when (or force (featurep 'mu4e-thread-folding))
            (set-face-attribute 'mu4e-thread-folding-root-folded-face nil :background nil :inherit nil)
            (set-face-attribute 'mu4e-thread-folding-root-unfolded-face nil :background nil :inherit 'highlight)
            (set-face-attribute 'mu4e-thread-folding-child-face nil :background nil :inherit 'region)))
      '';
      config = ''
        (general-define-key
          :keymaps 'mu4e-headers-mode-map
          "<tab>" 'mu4e-headers-toggle-at-point)
        (splinter-mu4e-thread-folding-mode-set-faces t)
      '';
      package = epkgs:
        epkgs.trivialBuild {
          pname = "mu4e-thread-folding";
          buildInputs = [
            (epkgs.trivialBuild {
              pname = "mu4e";
              src = "${pkgs.mu}/share/emacs/site-lisp/mu4e";
            })
          ];
          src = inputs.mu4e-thread-folding;
        };
      hook = [
        "(after-load-theme . splinter-mu4e-thread-folding-mode-set-faces)"
        "(mu4e-main-mode . mu4e-thread-folding-mode)"
      ];
    };
  };
}
