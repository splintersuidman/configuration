{ ... }:
{
  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: {
      inherit (tpkgs) scheme-full;
    };
  };

  programs.emacs.init.usePackage = {
    tex-mode = {
      enable = true;
      after = [ "evil-leader" ];
      extraConfig = ''
        :defines TeX-engine
      '';
      init = ''
        (setq TeX-engine 'xetex)
      '';
      config = ''
        (evil-leader/set-key-for-mode 'tex-mode
          "cc" 'TeX-command-master
          "cv" 'TeX-view)
      '';
    };
  };
}

