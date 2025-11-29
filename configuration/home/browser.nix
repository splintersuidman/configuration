{ pkgs, config, lib, ... }: {
  imports = [ ../../modules/home/browser.nix ];

  home.packages = [ pkgs.unstable.tor-browser ];

  programs.chromium = {
    enable = true;
    package = pkgs.chromium.override { enableWideVine = true; };
  };

  programs.browser = rec {
    enable = false;
    package = config.programs.firefox.package;
    program = "${package}/bin/firefox";
  };

  programs.firefox = {
    enable = true;
    package = pkgs.firefox;

    profiles = {
      default = {
        isDefault = true;

        extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
          browserpass
          darkreader
          decentraleyes
          greasemonkey
          languagetool
          multi-account-containers
          reddit-enhancement-suite
          refined-github
          tridactyl
          ublock-origin
          umatrix
        ];

        settings = {
          # Enable vertical tabs.
          "sidebar.verticalTabs" = true;

          # Enable HTTPS Only mode.
          "dom.security.https_only_mode" = true;

          # Adds MPRIS-support.
          "media.hardwaremediakeys.enabled" = true;

          # Enables user chrome configuration.
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

          # Set downloads directory.
          "browser.download.dir" = config.xdg.userDirs.download;

          "toolkit.telemetry.archive.enabled" = false;
          "toolkit.telemetry.enabled" = false;
          "toolkit.telemetry.rejected" = true;
          "toolkit.telemetry.server" = "";
          "toolkit.telemetry.unified" = false;
          "toolkit.telemetry.unifiedIsOptIn" = false;

          # Firefox tweaks suggested by privacytools.io.
          # The option privacy.resistFingerprinting interferes with
          # communicating the preferred colour scheme and time zone,
          # however, so we override these options.
          # See <https://superuser.com/a/1815927>.
          "privacy.firstparty.isolate" = false;
          "privacy.resistFingerprinting" = false;
          "privacy.fingerprintingProtection" = true;
          "privacy.fingerprintingProtection.overrides" =
            "+AllTargets,-CSSPrefersColorScheme,-JSDateTimeUTC";
          "privacy.trackingprotection.fingerprinting.enabled" = true;
          "privacy.trackingprotection.cryptomining.enabled" = true;
          "privacy.trackingprotection.enabled" = true;
          "browser.send_pings" = false;
          # "browser.sessionstore.max_tabs_undo" = 0;
          "browser.urlbar.speculativeConnect.enabled" = false;
          "dom.event.clipboardevents.enabled" = true;
          "media.eme.enabled" = false;
          "media.gmp-widevinecdm.enabled" = false;
          "media.navigator.enabled" = false;
          "network.cookie.cookieBehavior" = 1;
          "network.http.referer.XOriginPolicy" = 2;
          "network.http.referer.XOriginTrimmingPolicy" = 2;
          "webgl.disabled" = true;
          "browser.sessionstore.privacy_level" = 2;
          "beacon.enabled" = false;
          "browser.safebrowsing.downloads.remote.enabled" = false;
          "network.IDN_show_punycode" = true;
        };

        userChrome = ''
          @namespace url("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul");
        '';
      };
    };
  };
}
