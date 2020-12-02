{ ... }: {
  programs.emacs.init.usePackage = {
    eglot = {
      enable = true;
      after = [ "general" ];

      config = ''
        ;; TODO: doesn't seem to work.
        (general-define-key
          :prefix my-leader
          :states '(normal visual motion)
          :keymaps 'override
          "la" 'eglot-code-actions
          "lf" 'eglot-format
          "ll" 'eglot
          "lq" 'eglot-shutdown
          "lr" 'eglot-rename)
      '';
    };

    lsp-mode = {
      enable = false;
      after = [ "general" "flycheck" ];
      command = [ "lsp" ];
      init = ''
        ;; Prefer lsp-ui with flycheck over flymake.
        (setq lsp-prefer-flymake nil)
      '';
      config = ''
        (general-define-key
          :prefix my-leader
          :states '(normal visual motion)
          :keymaps 'override
          "ll" 'lsp)
      '';
    };

    lsp-ui = {
      enable = false;
      after = [ "general" ];
      command = [ "lsp-ui-mode" ];
      init = ''
        (setq lsp-ui-doc-enable nil)
        (setq lsp-ui-doc-position 'top)
      '';

      config = ''
        (general-define-key
          :prefix my-leader
          :states '(normal visual motion)
          :keymaps 'override
          "ld" 'lsp-ui-doc-glance
          "lgd" 'lsp-ui-peek-find-definitions
          "lm" 'lsp-ui-imenu)
      '';
    };
  };
}
