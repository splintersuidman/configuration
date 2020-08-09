{ config, ... }:
let
  documents = config.xdg.userDirs.documents;
in
{
  programs.emacs.init.usePackage = {
    zetteldeft = {
      enable = true;
      after = [ "evil-leader" ];
      init = ''
        (setq deft-directory "${documents}/notities")
        (setq zetteldeft-id-format "%Y-%m-%d-%H%M")
        (setq zetteldeft-id-regex "[0-9]\\{4\\}\\(-[0-9]\\{2,\\}\\)\\{3\\}")
        (setq zetteldeft-title-prefix "#+TITLE: ")
        ;; (setq zetteldeft-title-suffix "\n\n")
        (setq deft-extensions '("org" "md" "markdown" "txt" "text"))
        (setq deft-recursive t)

        (evil-leader/set-key
          "zd" 'deft
          "zD" 'zetteldeft-deft-new-search
          "zR" 'deft-refresh
          "zs" 'zetteldeft-search-at-point
          "zc" 'zetteldeft-search-current-id
          "zf" 'zetteldeft-follow-link
          "zF" 'zetteldeft-avy-file-search-ace-window
          "zl" 'zetteldeft-avy-link-search
          "zt" 'zetteldeft-avy-tag-search
          "zT" 'zetteldeft-tag-buffer
          "zi" 'zetteldeft-find-file-id-insert
          "zI" 'zetteldeft-find-file-full-title-insert
          "zo" 'zetteldeft-find-file
          "zn" 'zetteldeft-new-file
          "zN" 'zetteldeft-new-file-and-link
          "zr" 'zetteldeft-file-rename
          "zx" 'zetteldeft-count-words)
      '';
    };
  };
}
