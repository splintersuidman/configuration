(use-package emms
  :disabled
  :init
  (setq emms-seek-seconds 5)
  (setq emms-player-list '(emms-player-mpd))
  (setq emms-info-functions '(emms-info-mpd))
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all))
