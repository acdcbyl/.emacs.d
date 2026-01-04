;;; init-ui.el --- modeline,dashboard and icons -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; 使用nerd-icons作为图标包
(use-package nerd-icons
  :ensure t
  :when (display-graphic-p)
  :demand t)

;; 为其他窗口适配图标
(use-package nerd-icons-completion
  :ensure t
  :config
  (nerd-icons-completion-mode))

;; 为dired设置图标
(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; 设置doom-modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  ;; 你原来的设置保留大部分，只改下面这些
  (setq doom-modeline-height 36)                  ; 40 有点高，36-38 更协调
  (setq doom-modeline-bar-width 6)                ; 细条太瘦，加粗一点更有存在感
  (setq doom-modeline-window-width-limit fill-column)  ; 小窗口时自动收缩

  ;; 文件名显示更清晰
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)  ; 项目根 + 文件名，最实用
  ;; 或者 'relative-to-project 只显示项目内的相对路径

  ;; 图标和颜色
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t)

  ;; 简化一些不必要的显示，避免拥挤
  (setq doom-modeline-minor-modes nil)           ; 你已经关了，很好
  (setq doom-modeline-buffer-encoding 'nondefault)  ; 只在非 UTF-8 时显示，减少杂讯
  (setq doom-modeline-indent-info nil)           ; 一般不需要，除非你很在意 tab/spaces

  ;; 位置信息更简洁
  (setq doom-modeline-percent-position '(-3 "%p"))
  (setq doom-modeline-position-line-format '("%l"))
  (setq doom-modeline-position-column-format '(":%c"))

  ;; 让段落间距更舒服
  (setq doom-modeline-padded 8)  ; 默认 4，加大一点呼吸感更好

  ;; 可选：启用更现代的段落分隔符（斜线而不是竖线）
  (setq doom-modeline-modal t)                   ; 显示 modal 状态（normal/insert 等）
  (setq doom-modeline-modal-icon t)

  ;; 时间格式更美观（如果你要显示时间）
  (setq doom-modeline-time t
        doom-modeline-time-format " %H:%M ")
)


;; 可选：自定义颜色
;(custom-set-faces
 ;'(doom-modeline-buffer-modified ((t (:foreground "#ff6c6b" :weight bold))))
 ;'(doom-modeline-buffer-major-mode ((t (:foreground "#51afef" :weight bold))))
 ;'(doom-modeline-project-dir ((t (:foreground "#98be65" :weight bold))))
 ;'(doom-modeline-info ((t (:foreground "#51afef"))))
 ;'(doom-modeline-warning ((t (:foreground "#ECBE7B"))))
 ;'(doom-modeline-urgent ((t (:foreground "#ff6c6b")))))

;; 设置dashboard
(use-package dashboard
  :ensure t
  :init
  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons `(((,(if (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-mark_github") "★")
                                        "GitHub" "Browse" (lambda (&rest _) (browse-url homepage-url)))
                                       (,(if (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-download") "♺")
                                        "Upgrade" "Upgrade packages synchronously" (lambda (&rest _) (package-upgrade-all nil)) success))))
  (dashboard-setup-startup-hook)
  :config
  (defconst homepage-url "https://github.com/acdcbyl")
  :custom
  (dashboard-startup-banner "~/.face")
  (dashboard-image-banner-max-width 300)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-items '((recents   . 10)
                     (projects  . 7)))
  (dashboard-startupify-list '(dashboard-insert-banner
                               dashboard-insert-newline
                               dashboard-insert-banner-title
                               dashboard-insert-newline
                               dashboard-insert-navigator
                               dashboard-insert-newline
                               dashboard-insert-init-info
                               dashboard-insert-items
                               dashboard-insert-newline
                               dashboard-insert-footer)))

;; 设置good scroll
(use-package good-scroll
 :ensure t
 :if window-system     ; 在图形化界面时才使用这个插件
 :init (good-scroll-mode))
;; Colorize color names in buffers
(use-package colorful-mode
  :ensure t
  :diminish
  :hook (after-init . global-colorful-mode)
  :init (setq colorful-use-prefix t)
  :config (dolist (mode '(html-mode php-mode help-mode helpful-mode))
            (add-to-list 'global-colorful-modes mode)))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :ensure t
  :hook prog-mode)

;; 设置treemacs
(use-package treemacs
  :ensure t
  :bind ("C-c t" . treemacs))

(use-package treemacs-evil
  :ensure t
  :after (treemacs evil))

(use-package treemacs-nerd-icons
  :ensure t
  :config
  (treemacs-nerd-icons-config))

(provide 'init-ui)

;;; init-ui.el ends here
