;;; init-workspaces.el --- Workspace buffer -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Core Packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package
 tabspaces
 :ensure t
 :init
 (defun my--tabspace-setup ()
   "Set up tabspace at startup."
   ;; Add *Messages* and *splash* to Tab \`Home\'
   (tabspaces-mode 1)
   (progn
     (tab-bar-rename-tab "Home")
     (when (get-buffer "*Messages*")
       (set-frame-parameter
        nil 'buffer-list
        (cons
         (get-buffer "*Messages*")
         (frame-parameter nil 'buffer-list))))
     (when (get-buffer "*splash*")
       (set-frame-parameter
        nil 'buffer-list
        (cons
         (get-buffer "*splash*")
         (frame-parameter nil 'buffer-list))))))

 (add-hook 'after-init-hook #'my--tabspace-setup)
 :commands
 (tabspaces-switch-or-create-workspace
  tabspaces-open-or-create-project-and-workspace)
 :custom
 (tabspaces-use-filtered-buffers-as-default t)
 (tabspaces-default-tab "Default")
 (tabspaces-remove-to-default t)
 (tabspaces-include-buffers '("*scratch*"))
 (tabspaces-initialize-project-with-todo t)
 (tabspaces-todo-file-name "project-todo.org")
 ;; sessions
 (tabspaces-session t)
 (tabspaces-session-auto-restore t)
 ;; additional options
 (tabspaces-fully-resolve-paths t) ; Resolve relative project paths to absolute
 (tabspaces-exclude-buffers '("*Messages*" "*Compile-Log*")) ; Additional buffers to exclude
 (tab-bar-new-tab-choice "*scratch*")
 (tab-bar-show nil) ; 隐藏 tab-bar（可选）

 :config
 ;; Filter Buffers for Consult-Buffer
 (with-eval-after-load 'consult
   ;; hide full buffer list (still available with "b" prefix)
   (consult-customize consult--source-buffer :hidden t :default nil)
   ;; set consult-workspace buffer list
   (defvar consult--source-workspace
     (list
      :name "Workspace Buffers"
      :narrow ?w
      :history 'buffer-name-history
      :category 'buffer
      :state #'consult--buffer-state
      :default t
      :items
      (lambda ()
        (consult--buffer-query
         :predicate #'tabspaces--local-buffer-p
         :sort 'visibility
         :as #'buffer-name)))

     "Set workspace buffer list for consult-buffer.")
   (add-to-list 'consult-buffer-sources 'consult--source-workspace)))

(provide 'init-workspaces)
;;; init-workspaces.el ends here
