;;; init-music.el --- Bring music play for emacs -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Core Packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package mpd-modern
;;   :load-path "~/Workspace/Emacs-plugins/mpd-modern"
;;   :config
;;   (setq mpd-modern-host "localhost"
;;         mpd-modern-port 6600
;;         mpd-modern-music-directory "/home/aiser/Music/"
;;         mpd-modern-cover-cache-directory
;;         (expand-file-name "mpd-covers/" user-emacs-directory)))
(use-package
  mpdel
  :ensure t
  :commands (mpdel-mode mpdel-song-open)
  :defer t)
;; for embark
(use-package
  mpdel-embark
  :ensure t
  :after (embark mpdel)
  :config
  (progn
    (mpdel-embark-setup)))

(with-eval-after-load 'general
  (aiser/leader-def
    "m"   (list :wk (format "%s music" (nerd-icons-mdicon "nf-md-music_note")))
    "mb"  'mpdel-browser-open
    "ml"  'mpdel-playlist-open
    "ms"  'mpdel-song-open
    "mp"  'libmpdel-playback-play-pause
    "m]"  'libmpdel-playback-next
    "m["  'libmpdel-playback-previous
    "mS"  'libmpdel-stop
    "m+"  'libmpdel-volume-increase
    "m-"  'libmpdel-volume-decrease
    "mr"  'libmpdel-playback-set-random
    "mR"  'libmpdel-playback-set-repeat
    "mc"  'libmpdel-playlist-clear))

(provide 'init-music)
;;; init-music.el ends here
