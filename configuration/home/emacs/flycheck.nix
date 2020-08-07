{ ... }:
{
  programs.emacs.init.usePackage = {
    flycheck = {
      enable = true;
      hook = [
        "(c-mode . flycheck-mode)"
        "(c++-mode . flycheck-mode)"
      ];
    };

    flycheck-irony = {
      enable = false;
      after = [ "irony" "flycheck" ];
      hook = [
        "(flycheck-mode . flycheck-irony-setup)"
      ];
    };
  };
}
