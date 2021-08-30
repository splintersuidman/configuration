{ pkgs, config, ... }:
let documents = config.xdg.userDirs.documents;
in {
  programs.emacs.init.modules = {
    "init/init-org-mode.el" = {
      enable = true;
      config = ./org-mode.el;
      feature = "init-org-mode";
    };

    "init/init-org-mode-nix.el" = {
      enable = true;
      feature = "init-org-mode-nix";
      config = ''
        (use-package org
          :defer t
          :custom
          (org-agenda-files '("${documents}/agenda/")))

        (use-package org-roam
          :defer t
          :custom
          (org-roam-directory "${documents}/notities")

          ;; NOTE: not setting this causes an error, because
          ;;   (boundp 'emacsql-sqlite3-executable)
          ;;   => t
          ;; but
          ;;   emacsql-sqlite3-executable
          ;;   => nil
          ;; which makes
          ;;   (file-executable-p emacsql-sqlite3-executable)
          ;;   => error
          (emacsql-sqlite3-executable "${pkgs.sqlite}/bin/sqlite3")
          (org-roam-graph-executable "${pkgs.graphviz}/bin/dot"))

        (provide 'init-org-mode-nix)
      '';
    };
  };
}
