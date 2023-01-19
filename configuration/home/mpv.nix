{ pkgs, config, ... }: {
  programs.mpv = {
    enable = true;
    scripts = with pkgs.mpvScripts; [ mpris ];
    config = {
      ytdl-format =
        "(bestvideo[height<=720]+bestaudio)[ext=webm]/bestvideo[height<=720]+bestaudio/best[height<=720]/bestvideo+bestaudio/best";
      osd-font = "Iosevka Aile";
      keepaspect-window = false;
    };
  };
}
