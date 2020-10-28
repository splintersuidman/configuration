{ pkgs, config, ... }:
let
  agda = pkgs.agda.withPackages
    (agdaPkgs: with agdaPkgs; [ standard-library cubical agda-categories ]);
in {
  home.packages = [ agda ];

  home.file.".agda/defaults".text = ''
    standard-library
  '';

  programs.emacs.init.usePackage = {
    agda2-mode = {
      enable = true;
      after = [ "evil-leader" ];

      # The agda2-mode package in emacsPackages is often not the same version as
      # the agda2-mode Emacs package contained in the Agda package, so use the
      # version found by agda-mode locate and do not install another Emacs
      # package.
      package = _: null;
      extraConfig = ''
        :load-path
        (lambda ()
          (let ((coding-system-for-read 'utf-8))
            (file-name-directory (shell-command-to-string "${agda}/bin/agda-mode locate"))))
      '';

      mode = [
        ''("\\.agda\\'" . agda2-mode)''
        ''("\\.lagda.md\\'" . agda2-mode)''
        ''("\\.lagda.org\\'" . agda2-mode)''
        ''("\\.lagda.tex\\'" . agda2-mode)''
      ];

      config = ''
        (evil-leader/set-key-for-mode 'agda2-mode
          "c," 'agda2-goal-and-context
          "c." 'agda2-goal-and-context-and-inferred
          "c;" 'agda2-goal-context-and-checked
          "c=" 'agda2-show-constraints
          "c?" 'agda2-show-goals
          "ca" 'agda2-auto-maybe-all
          "cb" 'agda2-previous-goal
          "cc" 'agda2-make-case
          "cd" 'agda2-infer-type-maybe-toplevel
          "ce" 'agda2-show-context
          "cf" 'agda2-next-goal
          "ch" 'agda2-helper-function-type
          "cl" 'agda2-load
          "cn" 'agda2-compute-normalised-maybe-toplevel
          "co" 'agda2-module-contents-maybe-toplevel
          "cr" 'agda2-refine
          "cs" 'agda2-solve-maybe-all
          "ct" 'agda2-goal-type
          "cw" 'agda-why-in-scope-maybe-toplevel
          "cxa" 'agda2-abort
          "cxc" 'agda2-compile
          "cxd" 'agda2-remove-annotations
          "cxh" 'agda2-remove-implicit-arguments
          "cxl" 'agda2-load
          "cxq" 'agda2-quit
          "cxr" 'agda2-restart
          "cxs" 'agda2-set-program-version
          "cx;" 'agda2-comment-dwim-rest-of-buffer
          "cz" 'agda2-search-about-toplevel)

        ;; Make agda-mode not use default colours, but colours from the theme.
        (set-face-attribute 'agda2-highlight-datatype-face nil :inherit font-lock-type-face :foreground nil)
        (set-face-attribute 'agda2-highlight-field-face nil :inherit font-lock-function-name-face :foreground nil)
        (set-face-attribute 'agda2-highlight-function-face nil :inherit font-lock-function-name-face :foreground nil)
        (set-face-attribute 'agda2-highlight-inductive-constructor-face nil :inherit font-lock-type-face :foreground nil)
        (set-face-attribute 'agda2-highlight-keyword-face nil :inherit font-lock-keyword-face :foreground nil)
        (set-face-attribute 'agda2-highlight-module-face nil :inherit font-lock-type-face :foreground nil)
        (set-face-attribute 'agda2-highlight-number-face nil :inherit font-lock-type-face :foreground nil)
        (set-face-attribute 'agda2-highlight-operator-face nil :inherit font-lock-variable-name-face :foreground nil)
        (set-face-attribute 'agda2-highlight-postulate-face nil :inherit font-lock-type-face :foreground nil)
        (set-face-attribute 'agda2-highlight-primitive-face nil :inherit font-lock-function-name-face :foreground nil)
        (set-face-attribute 'agda2-highlight-primitive-type-face nil :inherit font-lock-type-face :foreground nil)
        (set-face-attribute 'agda2-highlight-record-face nil :inherit font-lock-type-face :foreground nil)
        (set-face-attribute 'agda2-highlight-symbol-face nil :inherit font-lock-variable-name-face :foreground nil)
      '';
    };
  };
}
