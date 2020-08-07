{ ... }:
{
  programs.emacs.init.usePackage = {
    switch-window = {
      enable = false;
      init = ''
        (setq switch-window-input-style 'minibuffer)
        (setq switch-window-increase 4)
        (setq switch-window-threshold 2)
        (setq switch-window-shortcut-style 'qwerty)
        (setq switch-window-qwerty-shortcuts
              '("j" "k" "l" "f" "d" "s" "n" "o" "i" "u" "e" "w"))
      '';
      # NOTE: this does not work, but I have no interest in making it
      # work.
      bind = {
        "[remap other-window]" = "switch-window";
      };
    };
  };
}
