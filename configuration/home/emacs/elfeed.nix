{ pkgs, lib, ... }:
let
  feed = url: tags:
    let urlString = ''"${url}"'';
    in if tags == [ ] then
      urlString
    else
      "(" + urlString + " " + lib.concatStringsSep " " tags + ")";
  feeds =
    # NOTE: This file is not contained in the git index.
    lib.concatStringsSep " " (import ./elfeed/feeds.nix { inherit feed; });

  mpv = "${pkgs.mpv}/bin/mpv";
in {
  programs.emacs.init.usePackage = {
    elfeed = {
      enable = true;
      after = [ "general" ];
      init = ''
        (setq elfeed-feeds '(${feeds}))
      '';
      config = ''
        (defun splinter-elfeed-mpv (entry)
          "Open the link of the currently selected item in mpv."
          (interactive (list (elfeed-search-selected :ignore-region)))
          (when (elfeed-entry-p entry)
            (elfeed-untag entry 'unread)
            (elfeed-search-update-entry entry)
            (make-process :name (elfeed-entry-title entry)
                          :command (list "${mpv}" (elfeed-entry-link entry)))))

        ;; Unbind leader key, as elfeed binds SPC.
        (general-define-key
          :prefix my-leader
          :states '(normal visual motion)
          :keymaps 'elfeed-search-mode-map
          "" nil)
        (general-define-key
          :prefix my-local-leader
          :states '(normal visual motion)
          :keymaps 'elfeed-search-mode-map
          "u" 'elfeed-update
          "v" 'splinter-elfeed-mpv)

        (set-face-attribute 'elfeed-search-feed-face nil :inherit font-lock-function-name-face :foreground nil)
        (set-face-attribute 'elfeed-search-tag-face nil :inherit font-lock-type-face :foreground nil)
        (set-face-attribute 'elfeed-search-date-face nil :inherit font-lock-variable-name-face :foreground nil)
      '';
    };
  };
}
