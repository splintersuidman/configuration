{ pkgs, config, ... }:
let eglotEnable = config.programs.emacs.init.usePackage.eglot.enable;
in {
  home.packages = [ pkgs.rustup ];

  programs.emacs.init.usePackage = {
    rust-mode = {
      enable = true;
      after = [ "general" ] ++ (if eglotEnable then [ "eglot" ] else [ ]);
      init = if eglotEnable then ''
        (add-to-list 'eglot-server-programs '(rust-mode . (eglot-rls "rls")))
      '' else
        "";
      config = ''
        (general-define-key
          :prefix my-local-leader
          :states '(normal visual motion)
          :keymaps 'rust-mode-map
          "f" 'rust-format-buffer
          "F" 'rust-format-diff-buffer)
      '';
    };
  };
}
