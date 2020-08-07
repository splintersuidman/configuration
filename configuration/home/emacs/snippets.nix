{ ... }:
{
  programs.emacs.init.usePackage = {
    yasnippet = {
      enable = true;
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
