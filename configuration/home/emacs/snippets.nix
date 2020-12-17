{ ... }:
{
  programs.emacs.init.usePackage = {
    yasnippet = {
      enable = false;
      init = ''
        (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
      '';

      config = ''
        (yas-reload-all)
        (yas-global-mode)
      '';
    };
  };
}
