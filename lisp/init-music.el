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
  :ensure t
  :config
  (require 'emms-setup)
  ;; 音乐目录
  (setq emms-source-file-default-directory "~/Music/")
  ;; 明确将播放器列表设置为只包含 MPD
  (setq emms-player-list '(emms-player-mpd))  
  ;; 使用缓存来加速
  (setq emms-cache-file "~/.emacs.d/emms/cache")
  
  ;; 异步加载封面，避免阻塞
  (setq emms-browser-covers 'emms-browser-cache-thumbnail-async)
  
  ;; 减少信息跟踪可以提速
  (setq emms-track-description-function 'emms-info-track-description)
  
  ;; 使用 MPD 可以显著提速（推荐）
  (require 'emms-player-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  
  ;; 如果使用 MPD，让 MPD 处理音乐库
  (setq emms-player-mpd-music-directory "~/Music/")
  
  ;; 延迟加载信息
  (setq emms-info-asynchronously t)
  (setq emms-mode-line nil)
  (setq emms-lyrics-display-on-modeline nil)
  (setq emms-lyrics-display-on-minibuffer t)
  (with-eval-after-load 'emms
  (custom-set-faces
   ;; 让 EMMS 继承主题的通用 face，这样换主题就自动适配了
   '(emms-browser-track-face ((t (:inherit default))))
   '(emms-playlist-track-face ((t (:inherit default))))
   '(emms-playlist-selected-face ((t (:inherit highlight :weight bold))))
   '(emms-browser-artist-face ((t (:inherit font-lock-function-name-face :weight bold))))
   '(emms-browser-album-face ((t (:inherit font-lock-type-face))))
   '(emms-browser-composer-face ((t (:inherit font-lock-variable-name-face))))
   '(emms-browser-year-face ((t (:inherit font-lock-comment-face))))
   '(emms-browser-track-number-face ((t (:inherit line-number))))))
)
  

;; 配置covers工具
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
