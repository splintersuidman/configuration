{ pkgs, ... }: {
  # Necessary for home-manager GTK configuration, see
  # <https://github.com/rycee/home-manager/blob/1afa5e257b71e1d27c6fc80ca38f344ea228bc1b/doc/faq.adoc#why-do-i-get-an-error-message-about-cadesrtdconf>.
  services.dbus.packages = [ pkgs.gnome3.dconf ];
}
