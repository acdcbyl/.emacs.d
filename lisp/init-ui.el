;;; init-ui.el --- modeline,dashboard and icons -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(load-file (expand-file-name "themes/catppuccin.el" user-emacs-directory))

;(use-package emacs
;  :config
;  (load-theme 'catppuccin t))          ; for light theme, use modus-operandi
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic nil)
  (doom-themes-neotree-file-icons t)
  ;; (doom-themes-treemacs-theme "doom-atom")
  :config
  (load-theme 'doom-nord-aurora t)
  (doom-themes-neotree-config)
  ;; (doom-themes-treemacs-config)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (with-eval-after-load 'neotree
    (defun my-neotree-root-style (node)
      (when (display-graphic-p)
	(insert
	 (concat
	  (insert "  ")
             (nerd-icons-octicon
              "nf-oct-stack"
              :height 1.0
	      :v-adjust -0.2
	      )
	     )))
  ;; insert project name
  (insert
   (propertize
    (concat " " (or (neo-path--file-truename node) "-")
              "\n")
      'display '(raise -0.1))))
  (advice-add 'doom-themes-neotree-insert-root :override #'my-neotree-root-style)
)
(with-eval-after-load 'neotree
    (add-hook 'neo-after-create-hook
              (lambda (_)
                (with-current-buffer (neo-global--get-buffer)
                  (face-remap-add-relative 'default
                                           :background (face-background 'mode-line nil t))))))
(with-eval-after-load 'treemacs
  (custom-set-faces
   ;; 使用 mode-line 的背景色
   `(treemacs-window-background-face 
     ((t (:background ,(face-attribute 'mode-line :background)))))
   ;; 高亮行用默认背景
   `(treemacs-hl-line-face 
     ((t (:background ,(face-attribute 'default :background)))))))
)

;; 使用nerd-icons作为图标包
(use-package nerd-icons
  :ensure t
  :when (display-graphic-p)
  :demand t)

;; 为其他窗口适配图标
(use-package nerd-icons-completion
  :ensure t
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode 1)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; 为dired设置图标
(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; 为ibuffer设置图标
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; 设置doom-modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  ;; 你原来的设置保留大部分，只改下面这些
  (setq doom-modeline-height 30)
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
)

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
  (dashboard-startup-banner "~/.config/nvim/lua/plugins/dashboard-img/129229269_p0_master1200.png")
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
;; (use-package treemacs
;;   :ensure t
;;   :commands (treemacs-follow-mode
;;              treemacs-filewatch-mode
;;              treemacs-git-mode)
;;   :custom-face
;;   (cfrs-border-color ((t (:inherit posframe-border))))
;;   :bind (([f8]        . treemacs)
;;          ("M-0"       . treemacs-select-window)
;;          ("C-x t 1"   . treemacs-delete-other-windows)
;;          ("C-x t t"   . treemacs)
;;          ("C-x t b"   . treemacs-bookmark)
;;          ("C-x t C-t" . treemacs-find-file)
;;          ("C-x t M-t" . treemacs-find-tag)
;;          :map treemacs-mode-map
;;          ([mouse-1]   . treemacs-single-click-expand-action))
;;   :config
;;   (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
;;         treemacs-missing-project-action  'remove
;;         treemacs-user-mode-line-format   'none
;;         treemacs-sorting                 'alphabetic-asc
;;         treemacs-follow-after-init       t
;;         treemacs-width                   30
;;         treemacs-no-png-images           nil)
;;   (treemacs-filewatch-mode t)
;;   (pcase (cons (not (null (executable-find "git")))
;;                (not (null (executable-find "python3"))))
;;     (`(t . t)
;;      (treemacs-git-mode 'deferred))
;;     (`(t . _)
;;      (treemacs-git-mode 'simple))))

;; (use-package treemacs-evil
;;   :ensure t
;;   :after (treemacs evil))

;; (use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
;;   :after (treemacs)
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Tabs))

;; (use-package treemacs-magit
;;   :after (treemacs magit)
;;   :ensure t)

;; (use-package treemacs-projectile
;;   :after (treemacs projectile)
;;   :ensure t)

;; (use-package treemacs-nerd-icons
;;   :ensure t
;;   :after (treemacs)
;;   :config
;;   (treemacs-nerd-icons-config))

;;设置neotree
(use-package neotree
  :ensure t
  :config
  (setq neo-smart-open t)
  (setq neo-theme (if (display-graphic-p) 'nerd-icons))
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-window-width 30)
  (setq neo-window-fixed-size t)
  (add-hook 'neotree-mode-hook ;; With evil
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
                (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
                (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
                (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
                (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
                (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle))))

;;设置tab
(use-package tab-bar
  :ensure nil ; 内置
  :config
  (setq tab-bar-close-button-show nil ; 隐藏关闭按钮
		tab-bar-new-button-show nil ; 隐藏新建按钮
)
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)))

(use-package centaur-tabs
  :ensure t
  :init
  (setq centaur-tabs-enable-key-bindings t)
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 38
        centaur-tabs-set-icons t
        centaur-tabs-show-new-tab-button t
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-set-bar 'under
        centaur-tabs-show-count nil
		centaur-tabs-icon-type 'nerd-icons  ; or 'nerd-icons
        ;; centaur-tabs-label-fixed-length 15
        ;; centaur-tabs-gray-out-icons 'buffer
        ;; centaur-tabs-plain-icons t
        x-underline-at-descent-line t
        centaur-tabs-left-edge-margin nil)
  (centaur-tabs-change-fonts (face-attribute 'default :font) 110)
  (centaur-tabs-headline-match)
  ;; (centaur-tabs-enable-buffer-alphabetical-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-S-<prior>" . centaur-tabs-move-current-tab-to-left)
  ("C-S-<next>" . centaur-tabs-move-current-tab-to-right)
  (:map evil-normal-state-map
        ("] t" . centaur-tabs-forward)
        ("[ t" . centaur-tabs-backward)))

(use-package spacious-padding
  :ensure t
  :hook (after-init . spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '( :internal-border-width 20      ; 整体内部边框（左右上会用这个）
      :internal-border-width-bottom 4 
      :internal-border-width-top 20    ; 上部保持宽敞
      :right-divider-width 16
      :fringe-width 12
      :mode-line-width 6))
  ;; (spacious-padding-subtle-mode-line t)
  )

;;隐藏modeline
(use-package hide-mode-line
  :ensure t
  :hook (((eat-mode
           eshell-mode shell-mode
           term-mode vterm-mode
           embark-collect-mode lsp-ui-imenu-mode
           pdf-annot-list-mode) . turn-on-hide-mode-line-mode)))


(provide 'init-ui)

;;; init-ui.el ends here
