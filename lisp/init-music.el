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
  (emms-all)
  (emms-default-players)
  ;; 音乐目录
  (setq emms-source-file-default-directory "~/Music/")
  
  ;; 使用缓存来加速
  (setq emms-cache-file "~/.emacs.d/emms/cache")
  (emms-cache 1)  ; 启用缓存
  
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
  )
  (setq emms-lyrics-display-on-modeline nil)
  (setq emms-lyrics-display-on-minibuffer t)

;; 配置covers工具
(use-package lyrics-fetcher
  :ensure t
  :after (emms)
  :config
  (setq lyrics-fetcher-genius-access-token "-qrPGzkNIyyD0MYV_VnxvO-iIZd8CmGKD_-SAUUKnGBKK-lEp3BHxnNWKkVC3NXw")
  :custom
  (lyrics-fetcher--generate-cover-sizes 'medium)
  (lyrics-fetcher-genius-download-cover t)
  (lyrics-fetcher-use-backend 'neteasecloud))

(provide 'init-music)
;;; init-music.el ends here
