{ pkgs, config, ... }:
let
  mu = "${pkgs.mu}/bin/mu";
  mu4eLoadPath = "${pkgs.mu}/share/emacs/site-lisp/mu4e";
  mbsync = "${config.programs.mbsync.package}/bin/mbsync";
in {
  home.packages = [
    pkgs.thunderbird
    pkgs.neomutt

    pkgs.mu
  ];

  imports = [
    # NOTE: This file is not contained in the git index.
    ./mail/accounts.nix
  ];

  programs.mbsync.enable = true;

  programs.emacs.init.usePackage = {
    mu4e = {
      enable = true;
      after = [ "evil-leader" ];
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
        (evil-leader/set-key-for-mode 'mu4e-headers-mode
          "cu" 'mu4e-update-mail-and-index)
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
  };
}
