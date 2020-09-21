{ pkgs, config, ... }:
let
  ccls = "${pkgs.ccls}/bin/ccls";
  eglotEnable = config.programs.emacs.init.usePackage.eglot.enable;
in {
  home.packages = [ pkgs.gcc pkgs.binutils pkgs.indent ];

  programs.emacs.init.usePackage = {
    cc-mode = {
      enable = true;
      after = if eglotEnable then [ "eglot" ] else [ ];
      init = ''
        (setq c-basic-offset 4)
        (setq c-default-style "k&r")
      '' + (if eglotEnable then ''
        (add-to-list 'eglot-server-programs '(c-mode . ("${ccls}")))
        (add-to-list 'eglot-server-programs '(c++-mode . ("${ccls}")))
      '' else
        "");

      hook = [
        ''
          (c-mode . (lambda () (define-key company-active-map (kbd "<tab>") 'company-complete)))''
        ''
          (c++-mode . (lambda () (define-key company-active-map (kbd "<tab>") 'company-complete)))''
      ];
    };
  };
}
