{ ... }: {
  programs.emacs.init.usePackage = {
    eglot = {
      enable = true;
      after = [ "evil-leader" ];

      config = ''
        (evil-leader/set-key
          "lf" 'eglot-format
          "lh" 'eglot-help-at-point
          "ll" 'eglot
          "lq" 'eglot-shutdown
          "lr" 'eglot-rename)
      '';
    };

    lsp-mode = {
      enable = false;
      after = [ "evil-leader" "flycheck" ];
      command = [ "lsp" ];
      init = ''
        ;; Prefer lsp-ui with flycheck over flymake.
        (setq lsp-prefer-flymake nil)
      '';
      config = ''
        (evil-leader/set-key
          "ll" 'lsp)
      '';
    };

    lsp-ui = {
      enable = false;
      after = [ "evil-leader" ];
      command = [ "lsp-ui-mode" ];
      init = ''
        (setq lsp-ui-doc-enable nil)
        (setq lsp-ui-doc-position 'top)
      '';

      config = ''
        (evil-leader/set-key
          "ld" 'lsp-ui-doc-glance
          "lgd" 'lsp-ui-peek-find-definitions
          "lm" 'lsp-ui-imenu)
      '';
    };
  };
}
