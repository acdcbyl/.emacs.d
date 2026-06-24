;;; init-workspaces.el --- Workspace management via tabspaces -*- lexical-binding: t -*-
;;; Commentary:
;; Uses tab-bar-mode + tabspaces for buffer-isolated workspace tabs.
;; Integrates with projectile for project-based workspaces.
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'transient))

;;;; Variables

(defvar my-main-workspace "Home"
  "Name of the primary workspace, which cannot be deleted.")

;;;; tab-bar configuration

(use-package tab-bar
  :ensure nil
  :commands (tab-bar-new-tab
             tab-bar-switch-to-tab
             tab-bar-switch-to-next-tab
             tab-bar-switch-to-prev-tab)
  :custom
  (tab-bar-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-close-tab-select 'recent)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (tab-bar-new-button nil)
  (tab-bar-close-button nil)
  (tab-bar-close-button-show nil)
  (tab-bar-auto-width nil)
  (tab-bar-format '(tab-bar-format-history
                    tab-bar-format-tabs
                    tab-bar-format-add-tab))
  :config
  (defun my-tab-bar-select-dwim ()
    "Select a tab. If only one tab exists, create one first."
    (interactive)
    (let ((tabs (mapcar (lambda (tab) (alist-get 'name tab))
                        (tab-bar--tabs-recent))))
      (cond ((null tabs) (tab-new))
            ((= (length tabs) 1) (tab-next))
            (t (tab-bar-switch-to-tab
                (completing-read "Select tab: " tabs nil t)))))))

;;;; tabspaces

(use-package tabspaces
  :ensure t
  :hook (emacs-startup . tabspaces-mode)
  :bind (:map projectile-command-map
              ("T" . my-tabspaces-open-project))
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab my-main-workspace)
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  :config
  (defun my-tabspaces-open-project ()
    "Open or create a workspace for a projectile project."
    (interactive)
    (let* ((project (projectile-completing-read "Project: "
                                                (projectile-relevant-known-projects))))
      (when project
        (tabspaces-open-or-create-project-and-workspace project))))

  (defun my-consult-tabspaces ()
    (require 'consult)
    (cond (tabspaces-mode
           (plist-put consult-source-buffer :hidden t)
           (plist-put consult-source-buffer :default nil)
           (add-to-list 'consult-buffer-sources 'consult--source-workspace))
          (t
           (plist-put consult-source-buffer :hidden nil)
           (plist-put consult-source-buffer :default t)
           (setq consult-buffer-sources
                 (remove 'consult--source-workspace consult-buffer-sources)))))
  (add-hook 'tabspaces-mode-hook #'my-consult-tabspaces))

;;;; Consult workspace source

(with-eval-after-load 'consult
  (plist-put consult-source-buffer :hidden t)
  (plist-put consult-source-buffer :default nil)
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                                :predicate #'tabspaces--local-buffer-p
                                :sort 'visibility
                                :as #'buffer-name)))
    "Workspace buffer source for consult-buffer."))

;;;; Workspace commands

(defun my/workspace-current-name ()
  "Return the current workspace tab name."
  (tabspaces--current-tab-name))

(defun my/workspace-list-names ()
  "Return a formatted string of all workspace names, highlighting the current one."
  (let ((current (my/workspace-current-name)))
    (mapconcat
     (lambda (name)
       (if (string= name current)
           (propertize (format "[%s]" name) 'face 'transient-value)
         name))
     (tabspaces--list-tabspaces)
     "  ")))

(defun my/workspace-new (name)
  "Create a new workspace with NAME."
  (interactive "sWorkspace name: ")
  (tab-new)
  (tab-bar-rename-tab name)
  (message "Created workspace: %s" name))

(defun my/workspace-switch ()
  "Switch to a workspace with completion."
  (interactive)
  (let* ((names (tabspaces--list-tabspaces))
         (name (completing-read "Switch to: " names nil t)))
    (tab-bar-switch-to-tab name)))

(defun my/workspace-kill ()
  "Delete the current workspace. The main workspace cannot be deleted."
  (interactive)
  (let ((name (my/workspace-current-name)))
    (if (string= name my-main-workspace)
        (message "Cannot delete the main workspace \"%s\"" my-main-workspace)
      (tab-close)
      (message "Deleted workspace: %s" name))))

(defun my/workspace-rename (name)
  "Rename the current workspace to NAME."
  (interactive "sNew name: ")
  (let ((old (my/workspace-current-name)))
    (tab-bar-rename-tab name)
    (message "Renamed workspace: %s \u2192 %s" old name)))

(defun my/workspace-switch-by-number (n)
  "Switch to the Nth workspace (1-indexed)."
  (let ((names (tabspaces--list-tabspaces)))
    (if (nth (1- n) names)
        (tab-bar-switch-to-tab (nth (1- n) names))
      (message "No workspace #%d" n))))

;;;; Transient suffix commands

(transient-define-suffix my/workspace-transient-new ()
  "Create a new workspace."
  :description "New workspace"
  (interactive)
  (let ((name (read-string "Workspace name: ")))
    (my/workspace-new name)))

(transient-define-suffix my/workspace-transient-switch ()
  "Switch to another workspace."
  :description "Switch workspace"
  (interactive)
  (my/workspace-switch))

(transient-define-suffix my/workspace-transient-kill ()
  "Delete the current workspace."
  :description "Delete workspace"
  (interactive)
  (let ((name (my/workspace-current-name)))
    (if (string= name my-main-workspace)
        (message "Cannot delete the main workspace \"%s\"" my-main-workspace)
      (when (yes-or-no-p (format "Delete workspace \"%s\"? " name))
        (my/workspace-kill)))))

(transient-define-suffix my/workspace-transient-rename ()
  "Rename the current workspace."
  :description "Rename workspace"
  (interactive)
  (let* ((old (my/workspace-current-name))
         (new (read-string (format "Rename \"%s\" to: " old))))
    (my/workspace-rename new)))

(transient-define-suffix my/workspace-transient-next ()
  "Switch to the next workspace."
  :description "Next workspace"
  (interactive)
  (tab-bar-switch-to-next-tab))

(transient-define-suffix my/workspace-transient-prev ()
  "Switch to the previous workspace."
  :description "Prev workspace"
  (interactive)
  (tab-bar-switch-to-prev-tab))

(transient-define-suffix my/workspace-transient-save ()
  "Save the current session."
  :description "Save session"
  (interactive)
  (tabspaces-save-session)
  (message "Session saved"))

(transient-define-suffix my/workspace-transient-load ()
  "Load a session."
  :description "Load session"
  (interactive)
  (tabspaces-restore-session)
  (message "Session loaded"))

(defmacro my/workspace-transient-define-switch-n (n)
  `(transient-define-suffix ,(intern (format "my/workspace-transient-switch-%d" n)) ()
     ,(format "Switch to workspace #%d" n)
     :description ,(format "#%d" n)
     (interactive)
     (my/workspace-switch-by-number ,n)))

(my/workspace-transient-define-switch-n 1)
(my/workspace-transient-define-switch-n 2)
(my/workspace-transient-define-switch-n 3)
(my/workspace-transient-define-switch-n 4)
(my/workspace-transient-define-switch-n 5)
(my/workspace-transient-define-switch-n 6)
(my/workspace-transient-define-switch-n 7)
(my/workspace-transient-define-switch-n 8)
(my/workspace-transient-define-switch-n 9)

;;;; Transient menu definition

(transient-define-prefix workspace-menu ()
  "Workspace management menu (tabspaces)."
  [:description
   (lambda ()
     (format "Workspaces: %s" (my/workspace-list-names)))
   ""]
  ["Navigate"
   :class transient-row
   ("<left>"  "\u2190 Prev"   my/workspace-transient-prev)
   ("<right>" "\u2192 Next"   my/workspace-transient-next)
   ("s"       "Switch"        my/workspace-transient-switch)]
  ["Switch by number"
   :class transient-row
   ("1" "#1" my/workspace-transient-switch-1)
   ("2" "#2" my/workspace-transient-switch-2)
   ("3" "#3" my/workspace-transient-switch-3)
   ("4" "#4" my/workspace-transient-switch-4)
   ("5" "#5" my/workspace-transient-switch-5)
   ("6" "#6" my/workspace-transient-switch-6)
   ("7" "#7" my/workspace-transient-switch-7)
   ("8" "#8" my/workspace-transient-switch-8)
   ("9" "#9" my/workspace-transient-switch-9)]
  ["Manage"
   ("n" "New"    my/workspace-transient-new)
   ("r" "Rename" my/workspace-transient-rename)
   ("k" "Delete" my/workspace-transient-kill)]
  ["Session"
   ("w" "Save" my/workspace-transient-save)
   ("l" "Load" my/workspace-transient-load)])

(provide 'init-workspaces)
;;; init-workspaces.el ends here
