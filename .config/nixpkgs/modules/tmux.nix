{ pkgs, config, ... }:
{
  programs.tmux = {
    enable = true;
    baseIndex = 0;
    clock24 = true;
    escapeTime = 0;
    keyMode = "vi";
    shortcut = "a";
    terminal = "xterm-256color";
    extraConfig = ''
      # For all keys (k) in {c, n, p}, use C-(k) as an alias for (k).
      bind C-c new-window
      bind c new-window
      bind C-n next-window
      bind n next-window
      bind C-p previous-window
      bind p previous-window

      bind a send-prefix

      bind v split-window -h
      bind s split-window -v
      bind J resize-pane -D 5
      bind K resize-pane -U 5
      bind H resize-pane -L 5
      bind L resize-pane -R 5
      bind h select-pane -L
      bind j select-pane -D
      bind k select-pane -U
      bind l select-pane -R

      bind x kill-pane

      bind r source-file ${config.home.homeDirectory}/.tmux.conf

      set -g mouse on
      set -g renumber-windows on

      set -g status-bg colour0
      set -g status-fg colour8
      set -g window-status-format " #I#F #W "
      set -g window-status-current-format " #[fg=green]#I#F #[fg=white,bold]#W #[fg=colour8]"
      set -g status-left ""
      set -g status-right ""
    '';
  };
}
