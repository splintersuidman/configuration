{ pkgs, config, inputs, ... }:
let
  mu = "${pkgs.mu}/bin/mu";
  mu4eLoadPath = "${pkgs.mu}/share/emacs/site-lisp/mu4e";
  mbsync = "${config.programs.mbsync.package}/bin/mbsync";
in {
  home.packages = [ pkgs.thunderbird pkgs.neomutt ];

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
      enable = true;
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

    mu4e-thread-folding = {
      enable = true;
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
          :mode 'mu4e-headers-mode
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
