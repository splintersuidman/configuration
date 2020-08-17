{ ... }:
{
  programs.emacs.init.usePackage = {
    lsp-mode = {
      enable = true;
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
      enable = true;
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
