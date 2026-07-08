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
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-indexing-method 'alien)
  (projectile-enable-caching t)
  (projectile-sort-order 'recently-active)
  (projectile-kill-buffers-filter 'kill-only-files)
  (projectile-project-search-path '("~/Code" "~/Projects"))
  (projectile-globally-ignored-directories
   '("node_modules"
     ".cache"
     ".direnv"
     ".venv"
     ".ruff_cache"
     ".pytest_cache"
     "dist"
     "build"
     "target"
     "bazel-bin"
     "bazel-out"
     "bazel-testlogs"))
  (projectile-globally-ignored-file-suffixes
   '(".eln" ".elc" ".pyc" ".o" ".so" ".a" ".swp"))
  (projectile-ignored-projects
   `("~/"
     "/tmp/"
     "/private/tmp/"
     ,package-user-dir)))

(provide 'init-project)

;;; init-project.el ends here
