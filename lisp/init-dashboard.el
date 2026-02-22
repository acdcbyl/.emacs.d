;;; init-dashboard.el --- dashboard -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

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
            (lambda (&rest _) (persp-load-state-from-file)))
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
  (dashboard-icon-type 'nerd-icons)
  (dashboard-page-separator "\f\n")
  (dashboard-path-style 'truncate-middle)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-projects-backend 'projectile)
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

(provide 'init-dashboard)

;;; init-dashboard.el ends here.
