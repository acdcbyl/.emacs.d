;;; init-workspaces.el --- Workspace management via persp-mode -*- lexical-binding: t -*-
;;; Commentary:
;; Adapted from Doom Emacs' workspaces module for vanilla Emacs configurations.
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-main-workspace "main"
  "Name of the primary workspace, which cannot be deleted.")

(defvar my--old-uniquify-style nil
  "Saved uniquify buffer name style before persp-mode was enabled, restored on disable.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; persp-mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package persp-mode
  :ensure t
  :hook (after-init . persp-mode)
  :config

  ;;; Basic settings
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-reset-windows-on-nil-window-conf nil
        persp-nil-hidden t
        persp-auto-save-fname "autosave"
        persp-save-dir (expand-file-name "workspaces/" user-emacs-directory)
        persp-set-last-persp-for-new-frames t
        persp-switch-to-added-buffer nil
        persp-kill-foreign-buffer-behaviour 'kill
        persp-remove-buffers-from-nil-persp-behaviour nil
        persp-auto-resume-time -1            ; don't auto-load session on startup
        persp-auto-save-opt (if noninteractive 0 1)) ; auto-save on exit

  ;;; Initialize the main workspace and fix the nil workspace issue
  ;; The nil perspective created by persp-mode has many quirks (buffers can't
  ;; be properly assigned to it, etc.), so we replace it with a real "main" workspace.
  (add-hook 'persp-mode-hook
            (defun my-workspaces-init-main-h ()
              (when persp-mode
                (let (persp-before-switch-functions)
                  (unless (or (persp-get-by-name my-main-workspace)
                              (> (hash-table-count *persp-hash*) 2))
                    (persp-add-new my-main-workspace)))
                ;; Ensure no frame ends up stuck in the nil perspective
                (dolist (frame (frame-list))
                  (when (string= (safe-persp-name (get-current-persp frame))
                                 persp-nil-name)
                    (persp-frame-switch my-main-workspace frame))))))

  ;; Run the same check after session restore, in case loading state brings back a nil persp
  (add-hook 'persp-after-load-state-functions
            (defun my-workspaces-ensure-no-nil-h (&rest _)
              (when persp-mode
                (dolist (frame (frame-list))
                  (when (string= (safe-persp-name (get-current-persp frame))
                                 persp-nil-name)
                    (persp-frame-switch (or (cadr (hash-table-keys *persp-hash*))
                                            my-main-workspace)
                                        frame))))))

  ;;; Fix uniquify compatibility
  ;; uniquify renames buffers, which breaks persp-mode's name-based buffer
  ;; serialization. Disable it while persp-mode is active, restore on disable.
  (add-hook 'persp-mode-hook
            (defun my-workspaces-fix-uniquify-h ()
              (if persp-mode
                  (progn
                    (when uniquify-buffer-name-style
                      (setq my--old-uniquify-style uniquify-buffer-name-style))
                    (setq uniquify-buffer-name-style nil))
                (when my--old-uniquify-style
                  (setq uniquify-buffer-name-style my--old-uniquify-style)))))

  ;;; Auto-register buffers into the current workspace
  ;; window-buffer-change-functions is more stable than after-change-major-mode-hook
  ;; and won't fire repeatedly on every major mode change.
  (add-hook 'window-buffer-change-functions
            (defun my-workspaces-add-buffer-h (&rest _)
              (when (and persp-mode
                         (not (minibufferp))
                         (not (persp-buffer-filtered-out-p
                               (or (buffer-base-buffer (current-buffer))
                                   (current-buffer))
                               persp-add-buffer-on-after-change-major-mode-filter-functions)))
                (persp-add-buffer (current-buffer) (get-current-persp) nil nil))))

  ;; Also register buffers opened via bookmarks, otherwise they won't appear
  ;; in the workspace buffer list
  (add-hook 'bookmark-after-jump-hook #'my-workspaces-add-buffer-h)

  ;;; Filter buffers that should not be persisted
  ;; Returning t means the buffer is filtered OUT (not saved).
  (add-hook 'persp-filter-save-buffers-functions
            (defun my-workspaces-dead-buffer-p (buf)
              "Filter out dead buffers."
              (not (buffer-live-p buf))))

  (add-hook 'persp-filter-save-buffers-functions
            (defun my-workspaces-remote-buffer-p (buf)
              "Filter out TRAMP remote buffers; they are very slow to restore."
              (let ((dir (buffer-local-value 'default-directory buf)))
                (ignore-errors (file-remote-p dir)))))

  ;;; Per-workspace winner-mode history
  ;; Each workspace independently saves and restores its window layout undo history.
  (with-eval-after-load 'winner
    (add-to-list 'window-persistent-parameters '(winner-ring . t))

    (add-hook 'persp-before-deactivate-functions
              (defun my-workspaces-save-winner-h (&rest _)
                (when (and (bound-and-true-p winner-mode)
                           (get-current-persp))
                  (set-persp-parameter
                   'winner-ring (list winner-currents
                                      winner-ring-alist
                                      winner-pending-undo-ring)))))

    (add-hook 'persp-activated-functions
              (defun my-workspaces-load-winner-h (&rest _)
                (when (bound-and-true-p winner-mode)
                  (cl-destructuring-bind (currents alist pending-undo-ring)
                      (or (persp-parameter 'winner-ring) '(nil nil nil))
                    (setq winner-undo-frame nil
                          winner-currents currents
                          winner-ring-alist alist
                          winner-pending-undo-ring pending-undo-ring))))))

  ;;; Fix evil visual selection lingering after switching workspaces
  (add-hook 'persp-before-deactivate-functions
            (lambda (&rest _) (deactivate-mark)))

  ;;; Persistence support for special buffer types
  ;; eshell
  (persp-def-buffer-save/load
   :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
   :save-vars '(major-mode default-directory))

  ;; compilation
  (persp-def-buffer-save/load
   :mode 'compilation-mode :tag-symbol 'def-compilation-buffer
   :save-vars '(major-mode default-directory compilation-directory
                           compilation-environment compilation-arguments))

  ;; magit (requires magit to be installed)
  (with-eval-after-load 'magit
    (persp-def-buffer-save/load
     :mode 'magit-status-mode :tag-symbol 'def-magit-status-buffer
     :save-vars '(default-directory)
     :load-function (lambda (savelist &rest _)
                      (cl-destructuring-bind (buffer-name vars &rest _)
                          (cdr savelist)
                        (magit-status (alist-get 'default-directory vars))))))

  ;;; Clean up posframe popups on session restore (if posframe is in use)
  (with-eval-after-load 'posframe
    (add-hook 'persp-after-load-state-functions
              (defun my-workspaces-delete-posframes-h (&rest _)
                (posframe-delete-all))))

  ;;; tab-bar integration (only active when tab-bar-mode is enabled)
  (add-hook 'tab-bar-mode-hook
            (defun my-workspaces-tab-bar-integration-h ()
              (add-hook 'persp-before-deactivate-functions
                        #'my-workspaces-save-tab-bar-data-h)
              (add-hook 'persp-activated-functions
                        #'my-workspaces-load-tab-bar-data-h)))

  (defun my-workspaces-save-tab-bar-data-h (&rest _)
    "Save the tab-bar state for the current workspace."
    (when (and (bound-and-true-p tab-bar-mode)
               (get-current-persp))
      (set-persp-parameter 'tab-bar-tabs (tab-bar-tabs))))

  (defun my-workspaces-load-tab-bar-data-h (&rest _)
    "Restore the tab-bar state for the current workspace."
    (when (bound-and-true-p tab-bar-mode)
      (when-let (tabs (persp-parameter 'tab-bar-tabs))
        (tab-bar-tabs-set tabs)
        (force-mode-line-update t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interactive commands
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/workspace-new ()
  "Create a new workspace and switch to it."
  (interactive)
  (let ((name (read-string "Workspace name: ")))
    (persp-add-new name)
    (persp-switch name)))

(defun my/workspace-switch ()
  "Switch to a workspace with completion."
  (interactive)
  (let* ((names (persp-names))
         (name (completing-read "Switch to: " names nil t)))
    (persp-switch name)))

(defun my/workspace-kill ()
  "Delete the current workspace. The main workspace cannot be deleted."
  (interactive)
  (let ((name (safe-persp-name (get-current-persp))))
    (if (string= name my-main-workspace)
        (message "Cannot delete the main workspace \"%s\"" my-main-workspace)
      (persp-kill name))))

(defun my/workspace-rename ()
  "Rename the current workspace."
  (interactive)
  (let* ((old-name (safe-persp-name (get-current-persp)))
         (new-name (read-string (format "Rename \"%s\" to: " old-name))))
    (persp-rename new-name)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Helper functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/workspace-current-name ()
  "Return the current workspace name for display in the transient header."
  (safe-persp-name (get-current-persp)))

(defun my/workspace-list-names ()
  "Return a formatted string of all workspace names, highlighting the current one."
  (let ((current (my/workspace-current-name)))
    (mapconcat
     (lambda (name)
       (if (string= name current)
           (propertize (format "[%s]" name) 'face 'transient-value)
         name))
     (persp-names)
     "  ")))

(defun my/workspace-switch-by-number (n)
  "Switch to the Nth workspace (1-indexed)."
  (let ((names (persp-names)))
    (if (nth (1- n) names)
        (persp-switch (nth (1- n) names))
      (message "No workspace #%d" n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transient suffix commands
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(transient-define-suffix my/workspace-transient-new ()
  "Create a new workspace."
  :description "New workspace"
  (interactive)
  (let ((name (read-string "Workspace name: ")))
    (persp-add-new name)
    (persp-switch name)
    (message "Switched to new workspace: %s" name)))

(transient-define-suffix my/workspace-transient-switch ()
  "Switch to another workspace."
  :description "Switch workspace"
  (interactive)
  (let* ((names (persp-names))
         (name (completing-read "Switch to: " names nil t)))
    (persp-switch name)))

(transient-define-suffix my/workspace-transient-kill ()
  "Delete the current workspace."
  :description "Delete workspace"
  (interactive)
  (let ((name (my/workspace-current-name)))
    (if (string= name my-main-workspace)
        (message "Cannot delete the main workspace \"%s\"" my-main-workspace)
      (when (yes-or-no-p (format "Delete workspace \"%s\"? " name))
        (persp-kill name)
        (message "Deleted workspace: %s" name)))))

(transient-define-suffix my/workspace-transient-rename ()
  "Rename the current workspace."
  :description "Rename workspace"
  (interactive)
  (let* ((old (my/workspace-current-name))
         (new (read-string (format "Rename \"%s\" to: " old))))
    (persp-rename new)
    (message "Renamed workspace: %s → %s" old new)))

(transient-define-suffix my/workspace-transient-next ()
  "Switch to the next workspace."
  :description "Next workspace"
  (interactive)
  (let* ((names (persp-names))
         (current (my/workspace-current-name))
         (idx (cl-position current names :test #'string=))
         (next (nth (mod (1+ idx) (length names)) names)))
    (persp-switch next)
    (message "Workspace: %s" next)))

(transient-define-suffix my/workspace-transient-prev ()
  "Switch to the previous workspace."
  :description "Prev workspace"
  (interactive)
  (let* ((names (persp-names))
         (current (my/workspace-current-name))
         (idx (cl-position current names :test #'string=))
         (prev (nth (mod (1- idx) (length names)) names)))
    (persp-switch prev)
    (message "Workspace: %s" prev)))

(transient-define-suffix my/workspace-transient-save ()
  "Save the current session to file."
  :description "Save session"
  (interactive)
  (persp-save-state-to-file)
  (message "Session saved to %s" persp-save-dir))

(transient-define-suffix my/workspace-transient-load ()
  "Load a session from file."
  :description "Load session"
  (interactive)
  (persp-load-state-from-file)
  (message "Session loaded"))

(transient-define-suffix my/workspace-transient-add-buffer ()
  "Add the current buffer to the current workspace."
  :description "Add buffer to workspace"
  (interactive)
  (persp-add-buffer (current-buffer))
  (message "Added buffer \"%s\" to workspace \"%s\""
           (buffer-name) (my/workspace-current-name)))

(transient-define-suffix my/workspace-transient-remove-buffer ()
  "Remove the current buffer from the current workspace."
  :description "Remove buffer from workspace"
  (interactive)
  (persp-remove-buffer (current-buffer))
  (message "Removed buffer \"%s\" from workspace \"%s\""
           (buffer-name) (my/workspace-current-name)))

(transient-define-suffix my/workspace-transient-switch-buffer ()
  "Switch to a buffer within the current workspace."
  :description "Switch buffer (workspace)"
  (interactive)
  (let* ((bufs (mapcar #'buffer-name (persp-buffers (get-current-persp))))
         (name (completing-read "Buffer: " bufs nil t)))
    (switch-to-buffer name)))

;; Switch to workspace by number (1-9)
(defmacro my/workspace-transient-define-switch-n (n)
  `(transient-define-suffix ,(intern (format "my/workspace-transient-switch-%d" n)) ()
     ,(format "Switch to workspace #%d" n)
     :description ,(format "Workspace #%d" n)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transient menu definition
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(transient-define-prefix my/workspace-menu ()
  "Workspace management menu (persp-mode)."
  [:description
   (lambda ()
     (format "Workspaces: %s" (my/workspace-list-names)))
   ;; Blank padding line
   ""]
  ["Navigate"
   :class transient-row
   ("<left>"  "← Prev"   my/workspace-transient-prev)
   ("<right>" "→ Next"   my/workspace-transient-next)
   ("s"       "Switch"   my/workspace-transient-switch)]
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
  ["Buffers"
   ("b" "Switch buffer"  my/workspace-transient-switch-buffer)
   ("a" "Add buffer"     my/workspace-transient-add-buffer)
   ("x" "Remove buffer"  my/workspace-transient-remove-buffer)]
  ["Session"
   ("w" "Save session" my/workspace-transient-save)
   ("l" "Load session" my/workspace-transient-load)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; projectile bridge
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package persp-mode-projectile-bridge
  :ensure t
  :after (persp-mode projectile)
  :config
  (add-hook 'persp-mode-projectile-bridge-mode-hook
            (lambda ()
              (if persp-mode-projectile-bridge-mode
                  (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
                (persp-mode-projectile-bridge-kill-perspectives))))
  (add-hook 'after-init-hook
            (lambda ()
              (persp-mode-projectile-bridge-mode 1))
            t))

(provide 'init-workspaces)
;;; init-workspaces.el ends here
