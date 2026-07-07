;;; init-ui.el --- Tab bar and minor UI tweaks -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

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
  :defer t)

;; Set up tab line
(use-package
  tab-bar
  :ensure nil
  :config
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (setq tab-bar-tab-close-button-show nil))

(use-package
  centaur-tabs
  :ensure t
  :init (setq centaur-tabs-enable-key-bindings t)
  :config
  (setq
   centaur-tabs-style "bar"
   centaur-tabs-height 35
   centaur-tabs-set-icons t
   centaur-tabs-show-new-tab-button nil
   centaur-tabs-set-modified-marker t
   centaur-tabs-show-navigation-buttons nil
   centaur-tabs-set-bar 'left
   centaur-tabs-set-close-button nil
   centaur-tabs-show-count nil
   centaur-tabs-icon-type 'nerd-icons
   centaur-tabs-gray-out-icons 'buffer
   ;; x-underline-at-descent-line t
   centaur-tabs-left-edge-margin nil)
  (centaur-tabs-change-fonts (face-attribute 'default :font) 100)
  (centaur-tabs-headline-match)
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
  (mu4e-org-mode . centaur-tabs-local-mode)
  (mu4e-main-mode . centaur-tabs-local-mode)
  (mu4e-dbus-mode . centaur-tabs-local-mode)
  (mu4e-view-mode . centaur-tabs-local-mode)
  (mu4e-thread-mode . centaur-tabs-local-mode)
  (mu4e-compose-mode . centaur-tabs-local-mode)
  (mu4e-headers-mode . centaur-tabs-local-mode)
  (mu4e-loading-mode . centaur-tabs-local-mode)
  (mu4e-raw-view-mode . centaur-tabs-local-mode)
  (ement-room-list-mode . centaur-tabs-local-mode)
  (ement-room-mode . centaur-tabs-local-mode)
  (ghostel-mode . centaur-tabs-local-mode)
  (after-init . centaur-tabs-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-S-<prior>" . centaur-tabs-move-current-tab-to-left)
  ("C-S-<next>" . centaur-tabs-move-current-tab-to-right))

(provide 'init-ui)
;;; init-ui.el ends here
