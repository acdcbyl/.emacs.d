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
;;  emacs
;;  :config
;;  (load-theme 'catppuccin t)) ; for light theme, use modus-operandi
;; (use-package
;;  kaolin-themes
;;  :ensure t
;;  :config
;;  (load-theme 'kaolin-galaxy t)
;;  (kaolin-treemacs-theme))
(use-package
 doom-themes
 :ensure t
 :custom (doom-themes-enable-bold t) (doom-themes-enable-italic t)
 :config (load-theme 'doom-tokyo-night t)
 ;; (doom-themes-neotree-config)
 ;; (doom-themes-treemacs-config)
 ;; (doom-themes-visual-bell-config)
 (doom-themes-org-config))


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
(use-package
 nerd-icons-dired
 :ensure t
 :hook (dired-mode . nerd-icons-dired-mode))

;; Set icons for ibuffer
(use-package
 nerd-icons-ibuffer
 :ensure t
 :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; Set up doom-modeline
(use-package
 doom-modeline
 :ensure t
 :hook (after-init . doom-modeline-mode)
 :custom
 (doom-modeline-irc nil)
 (doom-modeline-mu4e nil)
 (doom-modeline-gnus nil)
 (doom-modeline-github nil)
 (doom-modeline-persp-name nil)
 (doom-modeline-unicode-fallback t)
 (doom-modeline-enable-word-count nil))


;; Set up dashboard
(use-package
 dashboard
 :ensure t
 :init
 ;; Format: "(icon title help action face prefix suffix)"
 (setq dashboard-navigator-buttons
       `(((,(if (fboundp 'nerd-icons-octicon)
                (nerd-icons-octicon "nf-oct-mark_github")
              "★")
           "GitHub"
           "Browse"
           (lambda (&rest _) (browse-url homepage-url)))
          (,(if (fboundp 'nerd-icons-octicon)
                (nerd-icons-octicon "nf-oct-download")
              "♺")
           "Upgrade"
           "Upgrade packages synchronously"
           (lambda (&rest _) (package-upgrade-all nil))
           success))))
 (dashboard-setup-startup-hook)
 :config (defconst homepage-url "https://github.com/acdcbyl")
 :custom
 (dashboard-startup-banner
  "~/.config/nvim/lua/plugins/dashboard-img/129229269_p0_master1200.png")
 (dashboard-image-banner-max-width 300)
 (dashboard-set-heading-icons t)
 (dashboard-set-file-icons t)
 (dashboard-items '((recents . 10) (projects . 7)))
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

;; Colorize color names in buffers
(use-package
 colorful-mode
 :ensure t
 :diminish
 :hook (after-init . global-colorful-mode)
 :init (setq colorful-use-prefix t)
 :config
 (dolist (mode '(html-mode php-mode help-mode helpful-mode))
   (add-to-list 'global-colorful-modes mode)))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters :ensure t :hook prog-mode)

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
  dirvish-side-attributes '(vc-state nerd-iocns collapse file-size))
 ;; open large directory (over 20000 files) asynchronously with `fd' command
 (setq dirvish-large-directory-threshold 20000)
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

;;Set up tabs
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
  centaur-tabs-icon-type 'nerd-icons ; or 'nerd-icons
  ;; centaur-tabs-label-fixed-length 15
  ;; centaur-tabs-gray-out-icons 'buffer
  ;; centaur-tabs-plain-icons t
  x-underline-at-descent-line t
  centaur-tabs-left-edge-margin nil)
 (centaur-tabs-change-fonts (face-attribute 'default :font) 110)
 (centaur-tabs-headline-match)
 ;; (centaur-tabs-group-by-projectile-project)
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
 (eat-mode . centaur-tabs-local-mode)
 (mpdel-browser-mode . centaur-tabs-local-mode)
 (mpdel-tablist-mode . centaur-tabs-local-mode)
 (mpdel-playlist-mode . centaur-tabs-local-mode)
 ;; (eldoc-box-hover-mode . centaur-tabs-local-mode)
 (calendar-mode . centaur-tabs-local-mode)
 (org-agenda-mode . centaur-tabs-local-mode)
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
 :hook (after-init . spacious-padding-mode)
 :custom
 (spacious-padding-widths
  '(:internal-border-width
    20
    :internal-border-width-bottom 4
    :internal-border-width-top 20
    :right-divider-width 16
    :fringe-width 0
    :mode-line-width 6))
 ;; (spacious-padding-subtle-mode-line t)
 )

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
    embark-collect-mode
    quickrun--mode
    mpdel-browser-mode
    mpdel-tablist-mode
    mpdel-playlist-mode
    lsp-ui-imenu-mode
    pdf-annot-list-mode)
   . turn-on-hide-mode-line-mode)))

;; Customize popwin behavior
(use-package
 shackle
 :ensure t
 :hook (after-init . shackle-mode)
 :custom (shackle-default-size 0.4) (shackle-default-alignment 'below)
 (shackle-rules
  '((vc-annotate-mode :select t :inhibit-window-quit t :same t)
    ("*quickrun*"
     :select t
     :size 0.4
     :align below
     :popup t
     :inhibit-window-quit t)
    (profiler-report-mode :select t)
    (eat-mode :select t :inhibit-window-quit t :popup t)
    (xwidget-webkit-mode :select t :same t)
    (flycheck-error-list-mode :select t :align t :size 10)
    (comint-mode :select t :align t :size 0.4)
    (grep-mode :select t :align t)
    (rg-mode :select t :align t)
    ;; See also `help-window-select'
    (apropos-mode :select nil :align t :size 0.4)
    (help-mode :select nil :align t :size 0.4)
    ("*Backtrace*" :select t :align t :size 15)
    ("*Shell Command Output*" :select nil :align t :size 0.4)
    ("*Async Shell Command*" :select nil :align t :size 0.4)
    ("*Org-Babel Error Output*" :select nil :align t :size 0.3)
    ("*Process List*" :select t :align t :size 0.3)
    ("\\* MPDel\\ *"
     :regexp t
     ;; :inhibit-window-quit t
     :select t
     :size 0.4
     :align blow
     ;; :inhibit-window-quit
     :popup t)
    ("\\*mpdel-Current playlist\\*"
     :regexp t
     :select t
     :size 0.4
     :align blow
     :popup t)
    ("*Messages*" :select nil :size 0.25 :align below :popup t)
    ("*compilation*" :select t :size 0.3 :align below :popup t)
    ("*Occur*" :select t :align t))))

(provide 'init-ui)

;;; init-ui.el ends here
