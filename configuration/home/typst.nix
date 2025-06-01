{ pkgs, inputs, ... }: {
  home.packages = [
    pkgs.typst
    # TODO: use tinymist instead
    # pkgs.unstable.typst-lsp
  ];

  programs.emacs = {
    # overrides = self: super: {
    #   typst-ts-mode = self.trivialBuild {
    #     pname = "typst-ts-mode";
    #     src = inputs.typst-ts-mode;
    #     version = "0";
    #   };
    # };

    init.modules."init/init-typst.el" = {
      enable = false;
      config = ./typst.el;
      feature = "init-typst";
      extraPackages = epkgs: [ epkgs.typst-ts-mode ];
    };
  };
}
