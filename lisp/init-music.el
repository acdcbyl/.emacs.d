;;; init-music.el --- Bring music play for emacs -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Core Packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emms
  :defer t
  :ensure t
  :config
  (require 'emms-setup)
  ;; Music directory
  (setq emms-source-file-default-directory "~/Music/")
  ;; Explicitly set the player list to only include MPD
  ;; Use cache to speed up
  (emms-cache-enable)
  (setq emms-cache-file "~/.emacs.d/emms/cache")
  
  ;; Asynchronously load covers to avoid blocking
  (setq emms-browser-covers 'emms-browser-cache-thumbnail-async)
  
  ;; Reducing information tracking can speed things up
  (setq emms-track-description-function 'emms-info-track-description)
  
  ;; Using MPD can significantly speed things up (recommended)
  (require 'emms-player-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  
  ;; If using MPD, let MPD handle the music library
  (setq emms-player-mpd-music-directory "~/Music/")
  
  ;; Delay loading information
  (setq emms-info-asynchronously t)
  (setq emms-mode-line nil)
  (setq emms-lyrics-display-on-modeline nil)
  (setq emms-lyrics-display-on-minibuffer t)
  (with-eval-after-load 'emms
  (custom-set-faces
   ;; Let EMMS inherit the theme's common face, so that it will automatically adapt when changing themes
   '(emms-browser-track-face ((t (:inherit default))))
   '(emms-playlist-track-face ((t (:inherit default))))
   '(emms-playlist-selected-face ((t (:inherit highlight :weight bold))))
   '(emms-browser-artist-face ((t (:inherit font-lock-function-name-face :weight bold))))
   '(emms-browser-album-face ((t (:inherit font-lock-type-face))))
   '(emms-browser-composer-face ((t (:inherit font-lock-variable-name-face))))
   '(emms-browser-year-face ((t (:inherit font-lock-comment-face))))
   '(emms-browser-track-number-face ((t (:inherit line-number))))))
)
  

;; Configure covers tool
(use-package lyrics-fetcher
  :ensure t
  :after (emms)
  :config
  ;; The token should be set in a private, untracked file (e.g., lisp/private.el)
  ;; For example: (setq lyrics-fetcher-genius-access-token "YOUR_TOKEN_HERE")
  :custom
  (lyrics-fetcher--generate-cover-sizes 'medium)
  (lyrics-fetcher-use-backend 'neteasecloud))

(provide 'init-music)
;;; init-music.el ends here
