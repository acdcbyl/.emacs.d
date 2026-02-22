;;; init-ui.el --- modeline,themes and icons -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package
;;   doom-themes
;;   :load-path "~/.emacs.d/themes/doom-themes-matugen/"
;;   :config (load-theme 'doom-matugen t))
;; (use-package
;;   catppuccin-theme
;;   :load-path "~/Workspace/Emacs-plugins/emacs/"
;;   :config (load-theme 'catppuccin t))

(use-package
  doom-themes
  :vc (:url "https://github.com/acdcbyl/doom-themes-matugen" :rev :newest)
  :custom (doom-themes-enable-bold t) (doom-themes-enable-italic t)
  :config (load-theme 'doom-matugen t)
  ;; (doom-themes-neotree-config)
  ;; (doom-themes-treemacs-config)
  ;; (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Add solaire-mode for better ui.
(use-package
  solaire-mode
  :ensure t
  :hook ((after-init . solaire-global-mode)
         (dashboard-mode . turn-off-solaire-mode)))

;; Set up breadcrumb
;; (use-package
;;   breadcrumb
;;   :ensure t
;;   :defer t
;;   :hook (prog-mode . breadcrumb-mode)
;;   :custom
;;   (breadcrumb-project-crumb-separator " > ")
;;   (breadcrumb-imenu-crumb-separator " > ")
;;   :custom-face
;;   (breadcrumb-project-crumbs-face ((t (:height 0.9 :weight bold))))
;;   (breadcrumb-imenu-crumbs-face ((t (:height 0.9 :weight bold))))
;;   (breadcrumb-project-base-face ((t (:height 0.9 :weight bold))))
;;   (breadcrumb-imenu-leaf-face ((t (:height 0.9 :weight bold))))
;;   :config
;;   ;; Add icons.See also https://github.com/joaotavora/breadcrumb/issues/6
;;   (advice-add #'breadcrumb--format-project-node :around
;;               (lambda (og p more &rest r)
;;                 "Icon For File"
;;                 (let ((string (apply og p more r)))
;;                   (if (not more)
;;                       (concat (nerd-icons-icon-for-file string)
;;                               " " string)
;;                     (concat (nerd-icons-faicon
;;                              "nf-fa-folder_open"
;;                              :face 'breadcrumb-project-crumbs-face)
;;                             " "
;;                             string)))))

;;   (advice-add #'breadcrumb--project-crumbs-1 :filter-return
;;               (lambda (return)
;;                 "Icon for Parent Node"
;;                 (if (listp return)
;;                     (setf (car return)
;;                           (concat
;;                            " "
;;                            (nerd-icons-faicon
;;                             "nf-fa-rocket"
;;                             :face 'breadcrumb-project-base-face)
;;                            " "
;;                            (car return))))
;;                 return))

;;   (advice-add #'breadcrumb--format-ipath-node :around
;;               (lambda (og p more &rest r)
;;                 "Icon for items"
;;                 (let ((string (apply og p more r)))
;;                   (if (not more)
;;                       (concat (nerd-icons-codicon
;;                                "nf-cod-symbol_field"
;;                                :face 'breadcrumb-imenu-leaf-face)
;;                               " " string)
;;                     (cond ((string= string "Packages")
;;                            (concat (nerd-icons-codicon "nf-cod-package" :face 'breadcrumb-imenu-crumbs-face) " " string))
;;                           ((string= string "Requires")
;;                            (concat (nerd-icons-codicon "nf-cod-file_submodule" :face 'breadcrumb-imenu-crumbs-face) " " string))
;;                           ((or (string= string "Variable") (string= string "Variables"))
;;                            (concat (nerd-icons-codicon "nf-cod-symbol_variable" :face 'breadcrumb-imenu-crumbs-face) " " string))
;;                           ((string= string "Function")
;;                            (concat (nerd-icons-mdicon "nf-md-function_variant" :face 'breadcrumb-imenu-crumbs-face) " " string))
;;                           (t string))))))
;;   )

;; Set up doom-modeline
(use-package
  doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-minor-modes t)
  (doom-modeline-irc nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-gnus nil)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-enable-word-count nil))

(use-package minions
  :ensure t
  :config (minions-mode 1))

;;Hide modeline
(use-package
  hide-mode-line
  :ensure t
  :hook
  (((eat-mode
     eshell-mode
     shell-mode
     term-mode
     vterm-mode
     helpful-mode
     embark-collect-mode
     quickrun--mode
     mpdel-browser-mode
     mpdel-tablist-mode
     mpdel-playlist-mode
     mpdel-song-mode
     lsp-ui-imenu-mode
     pdf-view-mode
     pdf-annot-list-mode)
    . turn-on-hide-mode-line-mode)))

;; Colorize color names in buffers
(use-package
  colorful-mode
  :ensure t
  :diminish
  :hook (after-init . global-colorful-mode)
  :init (setq colorful-use-prefix t)
  :config
  (dolist (mode '(html-mode php-mode emacs-lisp-mode help-mode helpful-mode))
    (add-to-list 'global-colorful-modes mode)))

;; Add git diff in fringe
(use-package diff-hl
  :ensure t
  :defines diff-hl-show-hunk-posframe-internal-border-color
  :commands (diff-hl-flydiff-mode diff-hl-margin-mode)
  :custom-face
  (diff-hl-change ((t (:inherit custom-changed :foreground unspecified :background unspecified))))
  (diff-hl-insert ((t (:inherit diff-added :background unspecified))))
  (diff-hl-delete ((t (:inherit diff-removed :background unspecified))))
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (after-load-theme . diff-hl-set-posframe-appearance))
  :custom
  (diff-hl-draw-borders nil)
  (diff-hl-update-async t)
  (diff-hl-global-modes '(not image-mode pdf-view-mode))
  (diff-hl-show-hunk-function (if (childframe-workable-p)
                                  'diff-hl-show-hunk-posframe
                                'diff-hl-show-hunk-inline))
  :init
  (defun diff-hl-set-posframe-appearance ()
    "Set appearance of diff-hl-posframe."
    (setq diff-hl-show-hunk-posframe-internal-border-color
          (face-background 'posframe-border nil t)))
  (diff-hl-set-posframe-appearance)
  :config
  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  ;; Thin indicators on fringe
  (defun my-diff-hl-fringe-bmp-function (_type _pos)
    "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
    (define-fringe-bitmap 'my-diff-hl-bmp
      (vector #b11111100)
      1 8
      '(center t)))
  (setq diff-hl-fringe-bmp-function 'my-diff-hl-fringe-bmp-function)

  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Fall back to the display margin since the fringe is unavailable in tty
  (unless (display-graphic-p) (diff-hl-margin-mode 1)))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters :ensure t :hook prog-mode)

;; More features help
(use-package
  helpful
  :ensure t
  :defer t
  )

;; (use-package dired-subtree :ensure t)

;;Set up tab line
(use-package
  tab-bar
  :ensure nil
  :config
  (setq
   tab-bar-close-button-show nil
   tab-bar-new-button-show nil)
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)))

(use-package
  centaur-tabs
  :ensure t
  :init (setq centaur-tabs-enable-key-bindings t)
  :config
  (setq
   centaur-tabs-style "bar"
   centaur-tabs-height 38
   centaur-tabs-set-icons t
   centaur-tabs-show-new-tab-button t
   centaur-tabs-set-modified-marker t
   centaur-tabs-show-navigation-buttons t
   centaur-tabs-set-bar 'under
   centaur-tabs-show-count nil
   centaur-tabs-icon-type 'nerd-icons ;
   ;; centaur-tabs-label-fixed-length 15
   centaur-tabs-gray-out-icons 'buffer
   ;; centaur-tabs-plain-icons t
   x-underline-at-descent-line t
   centaur-tabs-left-edge-margin nil)
  (centaur-tabs-change-fonts (face-attribute 'default :font) 100)
  (centaur-tabs-headline-match)
  ;; (centaur-tabs-enable-buffer-alphabetical-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  ;; (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  (setq centaur-tabs-excluded-prefixes
        (append '("PREVIEW" "*dirvish" " *Embed" " *transient" "*xref")
                centaur-tabs-excluded-prefixes))
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
           (memq
            major-mode
            '(magit-process-mode
              magit-status-mode
              magit-diff-mode
              magit-log-mode
              magit-file-mode
              magit-blob-mode
              magit-blame-mode)))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode help-mode))
       "Help")
      ((memq
        major-mode
        '(org-mode
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
  (navigel-tablist-mode . centaur-tabs-local-mode)
  (eat-mode . centaur-tabs-local-mode)
  (mpdel-browser-mode . centaur-tabs-local-mode)
  (mpdel-song-mode . centaur-tabs-local-mode)
  (mpdel-tablist-mode . centaur-tabs-local-mode)
  (dirvish-directory-view-mode . centaur-tabs-local-mode)
  (dirvish-special-preview-mode . centaur-tabs-local-mode)
  (dired-mode . centaur-tabs-local-mode)
  (elfeed-show-mode . centaur-tabs-local-mode)
  (elfeed-search-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  (mpdel-playlist-mode . centaur-tabs-local-mode)
  (magit-process-mode . centaur-tabs-local-mode)
  (magit-status-mode . centaur-tabs-local-mode)
  (magit-diff-mode . centaur-tabs-local-mode)
  (magit-log-mode . centaur-tabs-local-mode)
  (magit-file-mode . centaur-tabs-local-mode)
  (magit-blob-mode . centaur-tabs-local-mode)
  (magit-blame-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (pdf-view-mode . centaur-tabs-local-mode)
  (ement-room-list-mode . centaur-tabs-local-mode)
  (ement-room-mode . centaur-tabs-local-mode)
  (after-init . centaur-tabs-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-S-<prior>" . centaur-tabs-move-current-tab-to-left)
  ("C-S-<next>" . centaur-tabs-move-current-tab-to-right)
  (:map
   evil-normal-state-map
   ("] t" . centaur-tabs-forward)
   ("[ t" . centaur-tabs-backward)))

(use-package
  spacious-padding
  :ensure t
  :config
  ;; These are the default values, but I keep them here for visibility.
  ;; Also check `spacious-padding-subtle-frame-lines'.
  (setq spacious-padding-widths
        '(:internal-border-width
          15
          :header-line-width 2
          ;; :mode-line-width 6
          :custom-button-width 3
          :tab-width 2
          :right-divider-width 30
          :scroll-bar-width 0
          :fringe-width nil))
  (spacious-padding-mode 1)

  ;; Set a key binding if you need to toggle spacious padding.
  (define-key global-map (kbd "<f8>") #'spacious-padding-mode))

(provide 'init-ui)

;;; init-ui.el ends here
