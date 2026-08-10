;;; init-workspaces.el --- Workspace management via tabspaces -*- lexical-binding: t -*-
;;; Commentary:
;; Uses tab-bar-mode + tabspaces for buffer-isolated workspace tabs.
;; Integrates with project.el for project-based workspaces.
;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'transient)

;;;; Variables

(defvar aiser-main-workspace "Home"
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
  (tab-bar-close-last-tab-choice 'delete-frame)
  (tab-bar-auto-width nil)
  (tab-bar-format '(tab-bar-format-history
                    tab-bar-format-tabs))
  (tab-bar-tab-close-button-show nil)
  :config
  (defun aiser/tab-bar-select-dwim ()
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
  :bind (:map aiser-project-prefix-map
              ("T" . aiser/tabspaces-open-project))
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab aiser-main-workspace)
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-session t)
  (tabspaces-session-auto-restore nil)
  :config
  (defun aiser/tabspaces-open-project ()
    "Open or create a workspace for a project."
    (interactive)
    (let* ((project (completing-read "Project: "
                                     (project-known-project-roots))))
      (when project
        (tabspaces-open-or-create-project-and-workspace project))))

  (defun aiser/consult-tabspaces ()
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
  (add-hook 'tabspaces-mode-hook #'aiser/consult-tabspaces))

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

(defun aiser/workspace-current-name ()
  "Return the current workspace tab name."
  (tabspaces--current-tab-name))

(defun aiser/workspace-list-names ()
  "Return a formatted string of all workspace names, highlighting the current one."
  (let ((current (aiser/workspace-current-name)))
    (mapconcat
     (lambda (name)
       (if (string= name current)
           (propertize (format "[%s]" name) 'face 'transient-value)
         name))
     (tabspaces--list-tabspaces)
     "  ")))

(defun aiser/workspace-new (name)
  "Create a new workspace with NAME."
  (interactive "sWorkspace name: ")
  (tab-new)
  (tab-bar-rename-tab name)
  (message "Created workspace: %s" name))

(defun aiser/workspace-switch ()
  "Switch to a workspace with completion."
  (interactive)
  (let* ((names (tabspaces--list-tabspaces))
         (name (completing-read "Switch to: " names nil t)))
    (tab-bar-switch-to-tab name)))

(defun aiser/workspace-kill ()
  "Delete the current workspace. The main workspace cannot be deleted."
  (interactive)
  (let ((name (aiser/workspace-current-name)))
    (if (string= name aiser-main-workspace)
        (message "Cannot delete the main workspace \"%s\"" aiser-main-workspace)
      (tab-close)
      (message "Deleted workspace: %s" name))))

(defun aiser/workspace-rename (name)
  "Rename the current workspace to NAME."
  (interactive "sNew name: ")
  (let ((old (aiser/workspace-current-name)))
    (tab-bar-rename-tab name)
    (message "Renamed workspace: %s \u2192 %s" old name)))

(defun aiser/workspace-switch-by-number (n)
  "Switch to the Nth workspace (1-indexed)."
  (let ((names (tabspaces--list-tabspaces)))
    (if (nth (1- n) names)
        (tab-bar-switch-to-tab (nth (1- n) names))
      (message "No workspace #%d" n))))

;;;; Transient suffix commands

(transient-define-suffix aiser/workspace-transient-new ()
  "Create a new workspace."
  :description "New workspace"
  (interactive)
  (let ((name (read-string "Workspace name: ")))
    (aiser/workspace-new name)))

(transient-define-suffix aiser/workspace-transient-switch ()
  "Switch to another workspace."
  :description "Switch workspace"
  (interactive)
  (aiser/workspace-switch))

(transient-define-suffix aiser/workspace-transient-kill ()
  "Delete the current workspace."
  :description "Delete workspace"
  (interactive)
  (let ((name (aiser/workspace-current-name)))
    (if (string= name aiser-main-workspace)
        (message "Cannot delete the main workspace \"%s\"" aiser-main-workspace)
      (when (yes-or-no-p (format "Delete workspace \"%s\"? " name))
        (aiser/workspace-kill)))))

(transient-define-suffix aiser/workspace-transient-rename ()
  "Rename the current workspace."
  :description "Rename workspace"
  (interactive)
  (let* ((old (aiser/workspace-current-name))
         (new (read-string (format "Rename \"%s\" to: " old))))
    (aiser/workspace-rename new)))

(transient-define-suffix aiser/workspace-transient-next ()
  "Switch to the next workspace."
  :description "Next workspace"
  (interactive)
  (tab-bar-switch-to-next-tab))

(transient-define-suffix aiser/workspace-transient-prev ()
  "Switch to the previous workspace."
  :description "Prev workspace"
  (interactive)
  (tab-bar-switch-to-prev-tab))

(transient-define-suffix aiser/workspace-transient-save ()
  "Save the current session."
  :description "Save session"
  (interactive)
  (tabspaces-save-session)
  (message "Session saved"))

(transient-define-suffix aiser/workspace-transient-load ()
  "Load a session."
  :description "Load session"
  (interactive)
  (tabspaces-restore-session)
  (message "Session loaded"))

(defmacro aiser/workspace-transient-define-switch-n (n)
  `(transient-define-suffix ,(intern (format "aiser/workspace-transient-switch-%d" n)) ()
     ,(format "Switch to workspace #%d" n)
     :description ,(format "#%d" n)
     (interactive)
     (aiser/workspace-switch-by-number ,n)))

(aiser/workspace-transient-define-switch-n 1)
(aiser/workspace-transient-define-switch-n 2)
(aiser/workspace-transient-define-switch-n 3)
(aiser/workspace-transient-define-switch-n 4)
(aiser/workspace-transient-define-switch-n 5)
(aiser/workspace-transient-define-switch-n 6)
(aiser/workspace-transient-define-switch-n 7)
(aiser/workspace-transient-define-switch-n 8)
(aiser/workspace-transient-define-switch-n 9)

;;;; Transient menu definition

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
   ("l" "Load" aiser/workspace-transient-load)])

(provide 'init-workspaces)
;;; init-workspaces.el ends here
