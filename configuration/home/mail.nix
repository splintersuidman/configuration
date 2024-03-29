{ pkgs, config, inputs, ... }:
let
  mbsync = "${config.programs.mbsync.package}/bin/mbsync";
  msmtp = "${pkgs.msmtp}/bin/msmtp";
  mu = "${pkgs.mu}/bin/mu";
  mu4eLoadPath = "${pkgs.mu}/share/emacs/site-lisp/mu4e";
  notify-send = "${pkgs.libnotify}/bin/notify-send";
  notmuch = "${pkgs.notmuch}/bin/notmuch";
in {
  home.packages = [ pkgs.thunderbird ];

  programs.mbsync.enable = false;
  programs.mu.enable = false;
  programs.msmtp.enable = false;
  programs.notmuch = {
    enable = false;
    new.tags = [ "unread" "inbox" "notification" ];
  };

  programs.emacs.overrides = self: super: {
    notmuch-notify = self.trivialBuild {
      pname = "notmuch-notify";
      src = ./mail/notmuch-notify;
      packageRequires = [ self.notmuch ];
    };

    notmuch-update = self.trivialBuild {
      pname = "notmuch-update";
      src = ./mail/notmuch-update;
      packageRequires = [ self.notmuch ];
    };
  };

  programs.emacs.init.modules."init/init-mail.el" = {
    enable = false;
    config = ./mail.el;
    feature = "init-mail";
    extraPackages = epkgs: with epkgs; [ notmuch-notify notmuch-update ];
  };
}
