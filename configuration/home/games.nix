{ pkgs, ... }: {
  home.packages = [
    # pkgs.desmume
    # (pkgs.freeciv.override {
    #   gtkClient = true;
    #   sdlClient = false;
    # })
    # prismlauncher
    # pkgs.unstable.polymc
    pkgs.openjdk
    # (pkgs.openttd.overrideAttrs (attrs: {
    #   # Compile with XDG Base Directory support.
    #   configureFlags = attrs.configureFlags ++ [ "--with-xdg-basedir" ];
    #   buildInputs = attrs.buildInputs ++ [ pkgs.libxdg_basedir ];
    # }))
    # pkgs.scid-vs-pc
    pkgs.steam
  ];
}
