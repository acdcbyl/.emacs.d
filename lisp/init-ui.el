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

;; (use-package
;;  emacs
;;  :config
;;  (load-theme 'catppuccin t)) ; for light theme, use modus-operandi
(use-package
 doom-themes
 :ensure t
 :custom (doom-themes-enable-bold t) (doom-themes-enable-italic t)
 ;; (doom-themes-neotree-file-icons t)
 ;; (doom-themes-treemacs-theme "doom-atom")
 :config (load-theme 'doom-ayu-mirage t)
 ;; (doom-themes-neotree-config)
 ;; (doom-themes-treemacs-config)
 ;; (doom-themes-visual-bell-config)
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
          :v-adjust -0.2))))
     ;; insert project name
     (insert
      (propertize (concat
                   " "
                   (or (neo-path--file-truename node) "-") "\n")
                  'display '(raise -0.1))))
   (advice-add
    'doom-themes-neotree-insert-root
    :override #'my-neotree-root-style))
 (with-eval-after-load 'neotree
   (add-hook
    'neo-after-create-hook
    (lambda (_)
      (with-current-buffer (neo-global--get-buffer)
        (face-remap-add-relative
         'default
         :background (face-background 'mode-line nil t))))))
 (with-eval-after-load 'treemacs
   (custom-set-faces
    ;; Use the background color of the mode-line
    `(treemacs-window-background-face
      ((t (:background ,(face-attribute 'mode-line :background)))))
    ;; Highlight the current line with the default background
    `(treemacs-hl-line-face
      ((t (:background ,(face-attribute 'default :background))))))))


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
 :init (doom-modeline-mode 1)
 :config
 (setq doom-modeline-height 30)
 (setq doom-modeline-bar-width 6)
 (setq doom-modeline-window-width-limit fill-column)

 ;; Display file names more clearly
 (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
 ;; Or 'relative-to-project' to only display the relative path within the project

 ;; Icons and colors
 (setq
  doom-modeline-icon t
  doom-modeline-major-mode-icon t
  doom-modeline-major-mode-color-icon t
  doom-modeline-buffer-state-icon t
  doom-modeline-buffer-modification-icon t)

 ;; Simplify some unnecessary displays to avoid clutter
 (setq doom-modeline-minor-modes nil)
 (setq doom-modeline-buffer-encoding 'nondefault)
 (setq doom-modeline-indent-info nil)

 ;; More concise position information
 (setq doom-modeline-percent-position '(-3 "%p"))
 (setq doom-modeline-position-line-format '("%l"))
 (setq doom-modeline-position-column-format '(":%c"))

 ;; Make paragraph spacing more comfortable
 (setq doom-modeline-padded 8)

 ;; Optional: Enable more modern paragraph separators (slashes instead of vertical bars)
 (setq doom-modeline-modal t)
 (setq doom-modeline-modal-icon t))

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
 :init
 ;; (load-file (expand-file-name "elpa/dirvish-2.3.0/extensions/dirvish-yank.el" user-emacs-directory))
 ;; (load-file (expand-file-name "elpa/dirvish-2.3.0/extensions/dirvish-collapse.el" user-emacs-directory))
 ;; (load-file (expand-file-name "elpa/dirvish-2.3.0/extensions/dirvish-peek.el" user-emacs-directory))
 ;; (load-file (expand-file-name "elpa/dirvish-2.3.0/extensions/dirvish-vc.el" user-emacs-directory))
 ;; (load-file (expand-file-name "elpa/dirvish-2.3.0/extensions/dirvish-subtree.el" user-emacs-directory))
 ;; (load-file (expand-file-name "elpa/dirvish-2.3.0/extensions/dirvish-side.el" user-emacs-directory))
 (dirvish-override-dired-mode)
 :custom
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
 (term-mode . centaur-tabs-local-mode)
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
    :fringe-width 12
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
    lsp-ui-imenu-mode
    pdf-annot-list-mode)
   . turn-on-hide-mode-line-mode)))


(provide 'init-ui)

;;; init-ui.el ends here
