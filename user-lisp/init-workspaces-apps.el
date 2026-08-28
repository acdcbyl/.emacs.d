;;; init-workspaces-apps.el --- Specialized workspace apps -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;; This file contains heavy workspace application logic that is deferred
;; to improve startup performance.

;;; Code:

(require 'transient)
(require 'tabspaces)

;;;; Startup Setup

(defun aiser/workspace-setup-startup ()
  "Ensure Home workspace contains essential buffers like Messages and Dashboard.
This is run when this file is loaded, typically after an idle delay."
  (interactive)
  (tab-bar-rename-tab aiser-main-workspace)
  (dolist (buf-name '("*Messages*" "*dashboard*" "*scratch*"))
    (when-let* ((buf (get-buffer buf-name)))
      (set-frame-parameter nil
                           'buffer-list
                           (cons buf (frame-parameter nil 'buffer-list))))))

;; Run the setup immediately when this file is required (which will be idle-triggered)
(aiser/workspace-setup-startup)

;;;; Specialized workspace functions

(defun aiser/workspace-open-agenda ()
  "Open Org Agenda in its own workspace. Redo agenda if already open."
  (interactive)
  (if (member "Agenda" (tabspaces--list-tabspaces))
      (progn
        (tab-bar-switch-to-tab "Agenda")
        (when (get-buffer "*Org Agenda*")
          (switch-to-buffer "*Org Agenda*")
          (org-agenda-redo))
        (delete-other-windows))
    (progn
      (tab-bar-new-tab)
      (tab-bar-rename-tab "Agenda")
      (require 'org)
      (org-agenda-list)
      (delete-other-windows))))

(defun aiser/workspace-open-email ()
  "Open mu4e email in its own workspace to isolate email buffers."
  (interactive)
  (if (member "Email" (tabspaces--list-tabspaces))
      (progn
        (tab-bar-switch-to-tab "Email")
        (cond ((get-buffer "*mu4e-headers*")
               (switch-to-buffer "*mu4e-headers*"))
              ((get-buffer " *mu4e-main*")
               (switch-to-buffer " *mu4e-main*")
               (delete-other-windows))
              (t (mu4e))))
    (progn
      (tab-bar-new-tab)
      (tab-bar-rename-tab "Email")
      (mu4e)
      (delete-other-windows))))

(defun aiser/workspace-open-emacsd ()
  "Open emacs.d in its own workspace with Magit status."
  (interactive)
  (if (member "emacs.d" (tabspaces--list-tabspaces))
      (tab-bar-switch-to-tab "emacs.d")
    (progn
      (tab-bar-new-tab)
      (tab-bar-rename-tab "emacs.d")
      (find-file (expand-file-name "init.el" user-emacs-directory))
      (split-window-right)
      (other-window 1)
      (magit-status user-emacs-directory))))

(defun aiser/workspace-open-temp-sandbox ()
  "Create a new temporary project sandbox workspace for total buffer isolation."
  (interactive)
  (tab-bar-new-tab)
  (tab-bar-rename-tab "Sandbox")
  (let ((temp-dir "/tmp/emacs-sandbox/"))
    (unless (file-exists-p temp-dir)
      (make-directory temp-dir t))
    (unless (file-exists-p (concat temp-dir ".git/"))
      (require 'magit)
      (magit-init temp-dir))
    (let ((default-directory temp-dir))
      (find-file (expand-file-name "scratch.md" temp-dir))
      (delete-other-windows))))

;;;; Update Transient Menu
;; We redefine the menu here to include the Apps section once this file is loaded.

(with-eval-after-load 'init-workspaces
  (transient-define-prefix workspace-menu ()
    "Workspace management menu (tabspaces)."
    [:description
     (lambda ()
       (format "Workspaces: %s" (aiser/workspace-list-names)))
     ""]
    ["Navigate"
     :class transient-row
     ("<left>"  "\u2190 Prev"   aiser/workspace-transient-prev)
     ("<right>" "\u2192 Next"   aiser/workspace-transient-next)
     ("s"       "Switch"        aiser/workspace-transient-switch)]
    ["Apps"
     ("a" "Org Agenda"    aiser/workspace-open-agenda)
     ("e" "Email (mu4e)"  aiser/workspace-open-email)
     ("c" "Emacs Config"  aiser/workspace-open-emacsd)
     ("x" "Temp Sandbox"  aiser/workspace-open-temp-sandbox)]
    ["Switch by number"
     :class transient-row
     ("1" "#1" aiser/workspace-transient-switch-1)
     ("2" "#2" aiser/workspace-transient-switch-2)
     ("3" "#3" aiser/workspace-transient-switch-3)
     ("4" "#4" aiser/workspace-transient-switch-4)
     ("5" "#5" aiser/workspace-transient-switch-5)
     ("6" "#6" aiser/workspace-transient-switch-6)
     ("7" "#7" aiser/workspace-transient-switch-7)
     ("8" "#8" aiser/workspace-transient-switch-8)
     ("9" "#9" aiser/workspace-transient-switch-9)]
    ["Manage"
     ("n" "New"    aiser/workspace-transient-new)
     ("r" "Rename" aiser/workspace-transient-rename)
     ("d" "Delete" aiser/workspace-transient-kill)]
    ["Session"
     ("w" "Save" aiser/workspace-transient-save)
     ("l" "Load" aiser/workspace-transient-load)]))

(provide 'init-workspaces-apps)
;;; init-workspaces-apps.el ends here
