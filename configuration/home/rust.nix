{ pkgs, config, ... }:
let eglotEnable = config.programs.emacs.init.usePackage.eglot.enable;
in {
  home.packages = [ pkgs.rustup ];

  programs.emacs.init.usePackage = {
    rust-mode = {
      enable = true;
      after = [ "evil-leader" ] ++ (if eglotEnable then [ "eglot" ] else [ ]);
      init = if eglotEnable then ''
        (add-to-list 'eglot-server-programs '(rust-mode . (eglot-rls "rls")))
      '' else
        "";
      config = ''
        (evil-leader/set-key-for-mode 'rust-mode
          "cf" 'rust-format-buffer
          "cF" 'rust-format-diff-buffer)
      '';
    };
  };
}
