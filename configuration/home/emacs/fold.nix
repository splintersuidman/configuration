{ ... }: {
  programs.emacs.init.usePackage = {
    origami = {
      enable = true;
      hook = [ "(prog-mode . origami-mode)" ];
    };
  };
}
