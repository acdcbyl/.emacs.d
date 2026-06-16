;;; init-project.el --- Add project support -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Project Config
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t
  :hook (after-init . projectile-mode)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (dolist (dir '("bazel-bin"
                 "bazel-out"
                 "bazel-testlogs"))
    (add-to-list 'projectile-globally-ignored-directories dir))
  :custom
  (projectile-use-git-grep t)
  (projectile-indexing-method 'alien)
  (projectile-kill-buffers-filter 'kill-only-files)
  ;; Ignore uninteresting files. It has no effect when using alien mode.
  (projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".swp" ".so" ".a"))
  (projectile-ignored-projects `("~/"
                                 "/tmp/"
                                 "/private/tmp/"
                                 ,package-user-dir)))
(provide 'init-project)

;;; init-project.el ends here
