{ ... }: {
  programs.emacs.init.usePackage = {
    eshell = {
      enable = true;
      after = [ "general" ];
      init = ''
        (defun eshell-new ()
          "Create a new eshell."
          (interactive)
          ;; The argument to `eshell' does not really matter, as long as it's
          ;; not an integer (n), which causes Emacs to open the nth `eshell'.
          (eshell 'new))
      '';
      hook = [''
        (eshell-mode .
          (lambda ()
            (general-define-key
              :prefix my-local-leader
              :keymaps 'eshell-mode-map
              :states '(normal visual motion)
              "h" 'counsel-esh-history)))
      ''];
    };
  };
}
