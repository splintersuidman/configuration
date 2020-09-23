{ ... }:
{
  programs.emacs.init.usePackage = {
    company = {
      enable = true;
      init = ''
        (setq company-idle-delay 0)
        (setq company-minimum-prefix-length 2)
      '';
      config = ''
        (global-company-mode 1)
      '';
      hook = [
        ''
          (prog-mode . (lambda () (company-mode 1)))
        ''
        ''
          (c-mode . (lambda ()
                      (delete 'company-semantic company-backends)))
        ''
      ];
      bindLocal = {
        company-active-map = {
          "M-n" = "nil";
          "M-p" = "nil";
          "C-n" = "company-select-next";
          "C-p" = "company-select-previous";
        };
      };
    };

    irony = {
      enable = false;
      hook = [
        "(c-mode . irony-mode)"
        "(c++-mode . irony-mode)"
        "(objc-mode . irony-mode)"
        "(irony-mode . irony-cdb-autosetup-compile-options)"
      ];
    };

    company-irony = {
      enable = false;
      after = [ "company" "irony" ];
      init = ''
        (add-to-list 'company-backends 'company-irony)
      '';
    };
  };
}
