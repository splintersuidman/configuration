(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (evil-leader/set-key-for-mode 'nix-mode
    "cc" 'nix-build
    "cr" 'nix-repl))

;; TODO: nix-company does not work (yet);
;; https://github.com/NixOS/nix-mode/issues/89
;; (require 'nix-company)
;; (add-to-list 'company-backends 'company-nix)
