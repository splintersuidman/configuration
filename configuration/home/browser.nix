{ pkgs, config, lib, ... }:
let
  colors = config.theme.base16.colors;
  rgb = base: "#${base.hex.rgb}";
  rgba = base: alpha:
    "rgba(${
      lib.concatMapStringsSep ", " toString [
        base.dec.r
        base.dec.g
        base.dec.b
        alpha
      ]
    })";
  rgbI = base: rgb base + " !important";
  rgbaI = base: alpha: rgba base alpha + " !important";
in
{
  imports = [
    ../../modules/home/browser.nix
  ];

  home.packages = [
    pkgs.torbrowser
    pkgs.qutebrowser
  ];

  programs.browser = rec {
    enable = true;
    package = config.programs.firefox.package;
    program = "${package}/bin/firefox";
  };

  programs.firefox = {
    enable = true;
    enableAdobeFlash = false;
    package = pkgs.firefox;

    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      browserpass
      darkreader
      decentraleyes
      greasemonkey
      https-everywhere
      multi-account-containers
      reddit-enhancement-suite
      torswitch
      ublock-origin
      umatrix
    ];

    profiles = {
      default = {
        isDefault = true;

        settings = {
          # Font settings.
          "font.name.monospace.x-western" = "Iosevka Cusotm";
          "font.name.sans-serif.x-western" = "Iosevka Aile";
          "font.name.serif.x-western" = "Iosevka Aile";

          # Adds MPRIS-support.
          "media.hardwaremediakeys.enabled" = true;

          # Enables user chrome configuration.
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          # Use devtools theme based on the base-16 theme.
          "devtools.theme" = "${config.theme.base16.kind}";
          "browser.display.background_color" = rgb colors.base00;
          "browser.display.foreground_color" = rgb colors.base05;

          # Set downloads directory.
          "browser.download.dir" = config.xdg.userDirs.download;

          "toolkit.telemetry.archive.enabled" = false;
          "toolkit.telemetry.enabled" = false;
          "toolkit.telemetry.rejected" = true;
          "toolkit.telemetry.server" = "";
          "toolkit.telemetry.unified" = false;
          "toolkit.telemetry.unifiedIsOptIn" = false;

          # Firefox tweaks suggested by privacytools.io.
          "privacy.firstparty.isolate" = true;
          "privacy.resistFingerprinting" = true;
          "privacy.trackingprotection.fingerprinting.enabled" = true;
          "privacy.trackingprotection.cryptomining.enabled" = true;
          "privacy.trackingprotection.enabled" = true;
          "browser.send_pings" = false;
          # "browser.sessionstore.max_tabs_undo" = 0;
          "browser.urlbar.speculativeConnect.enabled" = false;
          "dom.event.clipboardevents.enabled" = false;
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
  
        userContent = ''
          @-moz-document url(about:home), url(about:newtab) {
            body {
              --newtab-background-color: ${rgbI colors.base00};
              --newtab-element-hover-color: ${rgbI colors.base01};
              --newtab-icon-primary-color: ${rgbaI colors.base04 0.4};
              --newtab-search-border-color: ${rgbaI colors.base01 0.2};
              --newtab-search-dropdown-color: ${rgbI colors.base00};
              --newtab-search-dropdown-header-color: ${rgbI colors.base00};
              --newtab-search-icon-color: ${rgbaI colors.base04 0.4};
              --newtab-text-primary-color: ${rgbI colors.base05};
              --newtab-textbox-background-color: ${rgbI colors.base01};
              --newtab-textbox-border: ${rgbaI colors.base01 0.2};
              --newtab-topsites-background-color: ${rgbI colors.base04};
              --newtab-topsites-label-color: ${rgbI colors.base05};
            }
          }
        '';
      };
    };
  };
}
