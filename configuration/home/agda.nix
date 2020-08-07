{ pkgs, config, ... }:
let
  agda = pkgs.haskellPackages.Agda;
  stdlib = pkgs.AgdaStdlib;
in
{
  home.packages = [
    agda
    stdlib
  ];

  home.file.".agda/defaults".text = ''
    standard-library
  '';

  home.file.".agda/standard-library.agda-lib".text = ''
    name: standard-library
    include: ${stdlib}/share/agda
  '';

  home.file.".agda/libraries".text =
    let stdlibPath
        = (config.home.homeDirectory)
        + "/"
        + config.home.file.".agda/standard-library.agda-lib".target;
    in
    ''
      ${stdlibPath}
    '';

  # TODO: compile agda-mode. Maybe using a custom Emacs packages just
  # like programs.emacs.init does.
  programs.emacs.init.prelude = ''
    (load-file
     (let ((coding-system-for-read 'utf-8))
       (shell-command-to-string "${agda}/bin/agda-mode locate")))

    (setq auto-mode-alist
       (append
         '(("\\.agda\\'" . agda2-mode)
           ("\\.lagda.md\\'" . agda2-mode)
           ("\\.lagda.org\\'" . agda2-mode)
           ("\\.lagda.tex\\'" . agda2-mode))
         auto-mode-alist))

    (add-hook 'agda2-mode-hook
              (lambda ()
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
                  "cz" 'agda2-search-about-toplevel)))

    ;; Make agda-mode not use default colours, but colours from the theme.
    (add-hook 'agda2-mode-hook
      (lambda ()
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
        (set-face-attribute 'agda2-highlight-symbol-face nil :inherit font-lock-variable-name-face :foreground nil)))
  '';
}
