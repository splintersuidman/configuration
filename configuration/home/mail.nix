{ pkgs, config, ... }:
let
  mu = "${pkgs.mu}/bin/mu";
  mu4eLoadPath = "${pkgs.mu}/share/emacs/site-lisp/mu4e";
  mbsync = "${config.programs.mbsync.package}/bin/mbsync";
in {
  home.packages = [
    pkgs.thunderbird
    pkgs.neomutt
  ];

  imports = [
    # NOTE: This file is not contained in the git index.
    ./mail/accounts.nix
  ];

  programs.mu.enable = true;
  programs.mbsync.enable = true;

  programs.emacs.init.usePackage = {
    mu4e = {
      enable = true;
      after = [ "general" ];
      init = ''
        (setq mu4e-mu-binary "${mu}"

              mu4e-get-mail-command "${mbsync} --all"
              mu4e-update-interval (* 60 5)

              mu4e-sent-folder   "/sent"
              mu4e-drafts-folder "/drafts"
              mu4e-trash-folder  "/trash"
              mu4e-refile-folder "/archive")

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
      enable = true;
      after = [ "mu4e" ];
      init = ''
        (setq mu4e-alert-email-notification-types '(subjects))
      '';
      config = ''
        (mu4e-alert-set-default-style 'libnotify)
      '';
      hook = [
        ''(mu4e-main-mode . mu4e-alert-enable-notifications)''
      ];
    };

    mu4e-views = {
      enable = true;
      after = [ "mu4e" "general" ];
      init = ''
        (setq mu4e-views-completion-method 'ivy)
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
  };
}
