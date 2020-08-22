{ pkgs, ... }:
{
  programs.ssh.enable = true;
  xsession.profileExtra = ''
    eval $(${pkgs.openssh}/bin/ssh-agent)
  '';
}
