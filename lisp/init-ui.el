;;; init-ui.el --- modeline,dashboard and icons -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package
;;   catppuccin-theme
;;   :load-path "~/Workspace/Emacs-plugins/emacs/"
;;   :config (load-theme 'catppuccin t))

(use-package
  doom-themes
  :ensure t
  :custom (doom-themes-enable-bold t) (doom-themes-enable-italic t)
  :config (load-theme 'doom-tokyo-night t)
  ;; (doom-themes-neotree-config)
  ;; (doom-themes-treemacs-config)
  ;; (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Add solaire-mode for better ui.
(use-package
  solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1)
  :hook (dashboard-mode . turn-off-solaire-mode))

;; Use nerd-icons as the icon package
(use-package nerd-icons :ensure t :when (display-graphic-p) :demand t)

;; Adapt icons for other windows
(use-package
  nerd-icons-completion
  :ensure t
  :after (marginalia nerd-icons)
  :config (nerd-icons-completion-mode 1)
  (add-hook
   'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Set icons for dired
;; (use-package
;;  nerd-icons-dired
;;  :ensure t
;;  :hook (dired-mode . nerd-icons-dired-mode))

;; Set icons for ibuffer
(use-package
  nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

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
  (doom-modeline-irc nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-gnus nil)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-enable-word-count nil))

(defvar my-use-dashboard t
  "Enable dashboard.")

;; Set up dashboard
(use-package
  dashboard
  :ensure t
  :if my-use-dashboard
  :diminish dashboard-mode
  :bind
  (("<f2>" . open-dashboard)
   :map
   dashboard-mode-map
   ("q" . quit-dashboard)
   ("M-r" . restore-session))
  :hook (dashboard-mode . (lambda () (setq-local frame-title-format nil)))
  :init
  (setq dashboard-navigator-buttons
        `(((,(if (fboundp 'nerd-icons-octicon)
                 (nerd-icons-octicon "nf-oct-mark_github")
               )
            "GitHub"
            "Browse"
            (lambda (&rest _) (browse-url homepage-url)))
           (,(if (fboundp 'nerd-icons-octicon)
                 (nerd-icons-octicon "nf-oct-history")
               )
            "Restore"
            "Restore previous session"
            (lambda (&rest _) (restore-session)))
           (,(if (fboundp 'nerd-icons-octicon)
                 (nerd-icons-octicon "nf-oct-tools"))
            "Settings" "Open custom file"
            (lambda (&rest _) (find-file custom-file)))
           (,(if (fboundp 'nerd-icons-octicon)
                 (nerd-icons-octicon "nf-oct-download")
               )
            "Upgrade"
            "Upgrade packages synchronously"
            (lambda (&rest _) (package-upgrade-all nil))
            success))))
  (dashboard-setup-startup-hook)
  :config (defconst homepage-url "https://github.com/acdcbyl")

  ;; restore-session
  (defun restore-session ()
    "Restore the previous session."
    (interactive)
    (message "Restoring previous session...")
    (quit-window t)

    (when (fboundp 'tabspaces-mode)
      (unless tabspaces-mode
        (tabspaces-mode t))
      (tabspaces-restore-session))

    (message "Restoring previous session...done"))

  ;; recover layouts
  (defvar dashboard-recover-layout-p nil
    "Whether recovers the layout.")

  ;; open dashboard
  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    (if (length>
         (window-list-1)
         (if (and (fboundp 'treemacs-current-visibility)
                  (eq (treemacs-current-visibility) 'visible))
             2
           1))
        (setq dashboard-recover-layout-p t))

    (delete-other-windows)

    (dashboard-refresh-buffer))

  (defun quit-dashboard ()
    "Quit dashboard window."
    (interactive)
    (quit-window t)

    (when (fboundp 'tabspaces-mode)
      (unless tabspaces-mode
        (tabspaces-mode t)
        (tabspaces-switch-or-create-workspace tabspaces-default-tab)))

    (when dashboard-recover-layout-p
      (cond
       ((bound-and-true-p tab-bar-history-mode)
        (tab-bar-history-back))
       ((bound-and-true-p winner-mode)
        (winner-undo)))
      (setq dashboard-recover-layout-p nil)))
  :custom-face
  (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  (dashboard-items-face ((t (:weight normal))))
  (dashboard-no-items-face ((t (:weight normal))))
  :custom
  (dashboard-page-separator "\f\n")
  (dashboard-path-style 'truncate-middle)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-projects-backend 'project-el)
  (dashboard-path-style 'truncate-middle)
  (dashboard-path-max-length 60)
  (dashboard-startup-banner
   "~/.emacs.d/assets/GNUEmacs.png")
  (dashboard-image-banner-max-width 400)
  (dashboard-set-heading-icons t)
  ;; (dashboard-show-shortcuts nil)
  (dashboard-set-file-icons t)
  (dashboard-items '((recents . 10) (bookmarks . 5)(projects . 7)))
  (dashboard-startupify-list
   '(dashboard-insert-banner
     dashboard-insert-newline
     dashboard-insert-banner-title
     dashboard-insert-newline
     dashboard-insert-navigator
     dashboard-insert-newline
     dashboard-insert-init-info
     dashboard-insert-items
     dashboard-insert-newline
     dashboard-insert-footer)))

;; Display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :ensure t
  :defer t
  :diminish
  :hook (after-init . global-page-break-lines-mode)
  :config (dolist (mode '(dashboard-mode emacs-news-mode))
            (add-to-list 'page-break-lines-modes mode)))

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

;; Highlight brackets according to their depth
(use-package rainbow-delimiters :ensure t :hook prog-mode)

;; More features help
(use-package
  helpful
  :ensure t
  :defer t
  )

;;Dired beautification and enhancement
(use-package
  dired
  :config
  (setq
   dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  (put 'dired-find-alternate-file 'disabled nil))

(use-package
  dirvish
  :ensure t
  :init (dirvish-override-dired-mode)
  :custom (dirvish-side-width 30)
  (dirvish-window-fringe 0)
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/" "Home")
     ("d" "~/Downloads/" "Downloads")
     ("m" "/mnt/" "Drives")
     ("s" "/ssh:my-remote-server")
     "SSH server"
     ("e" "/sudo:root@localhost:/etc")
     "Modify program settings"
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  :config
  (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq
   dirvish-attributes ; The order *MATTERS* for some attributes
   '(vc-state subtree-state
              nerd-icons
              collapse
              git-msg
              file-time
              file-size)
   dirvish-side-attributes '(vc-state nerd-icons collapse file-size))
  ;; open large directory (over 20000 files) asynchronously with `fd' command
  (setq dirvish-large-directory-threshold 20000)
  (setq
   dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group --time-style=iso")
  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("h" . dired-up-directory) ; So you can adjust `dired' bindings here
   ("?" . dirvish-dispatch) ; [?] a helpful cheatsheet
   ("a" . dirvish-setup-menu) ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f" . dirvish-file-info-menu) ; [f]ile info
   ("o" . dirvish-quick-access) ; [o]pen `dirvish-quick-access-entries'
   ("s" . dirvish-quicksort) ; [s]ort flie list
   ("r" . dirvish-history-jump) ; [r]ecent visited
   ("l" . dirvish-ls-switches-menu) ; [l]s command flags
   ("v" . dirvish-vc-menu) ; [v]ersion control commands
   ("*" . dirvish-mark-menu)
   ("y" . dirvish-yank-menu)
   ("N" . dirvish-narrow)
   ("^" . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))

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
  (centaur-tabs-change-fonts (face-attribute 'default :font) 110)
  (centaur-tabs-headline-match)
  ;; (centaur-tabs-enable-buffer-alphabetical-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  ;; (centaur-tabs-mode t)
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
  (elfeed-show-mode . centaur-tabs-local-mode)
  (elfeed-search-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  (mpdel-playlist-mode . centaur-tabs-local-mode)
  (magit-diff-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
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
  (setq-default left-fringe-width 0) ;; close left fringe
  (spacious-padding-mode 1)

  ;; Set a key binding if you need to toggle spacious padding.
  (define-key global-map (kbd "<f8>") #'spacious-padding-mode))

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
     pdf-annot-list-mode)
    . turn-on-hide-mode-line-mode)))

(provide 'init-ui)

;;; init-ui.el ends here
