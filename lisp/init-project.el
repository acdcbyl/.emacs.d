;;; init-project.el --- Project support via project.el -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Project Config
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-project-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'project-find-file)
    (define-key map (kbd "F") #'project-or-external-find-file)
    (define-key map (kbd "b") #'project-switch-to-buffer)
    (define-key map (kbd "g") #'project-find-regexp)
    (define-key map (kbd "G") #'project-or-external-find-regexp)
    (define-key map (kbd "d") #'project-find-dir)
    (define-key map (kbd "D") #'project-dired)
    (define-key map (kbd "v") #'project-vc-dir)
    (define-key map (kbd "c") #'project-compile)
    (define-key map (kbd "p") #'project-switch-project)
    (define-key map (kbd "s") #'project-save-some-buffers)
    (define-key map (kbd "S") #'project-shell)
    (define-key map (kbd "k") #'project-kill-buffers)
    (define-key map (kbd "E") #'project-eshell)
    (define-key map (kbd "!") #'project-shell-command)
    (define-key map (kbd "&") #'project-async-shell-command)
    (define-key map (kbd "x") #'project-execute-extended-command)
    (define-key map (kbd "o") #'project-any-command)
    (define-key map (kbd "r") #'project-query-replace-regexp)
    map)
  "Project keymap mirroring project.el commands.")

(global-set-key (kbd "C-c p") my-project-prefix-map)

(use-package project
  :ensure nil
  :custom
  (project-list-file (expand-file-name "var/project-list.el" user-emacs-directory))
  (project-vc-ignores
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
     "bazel-testlogs"
     "*.eln"
     "*.elc"
     "*.pyc"
     "*.o"
     "*.so"
     "*.a"
     "*.swp"))
  :config
  ;; Automatic project root detection for LSP/project markers
  (defun my-project-try-lsp (dir)
    "Find project root by looking for project marker files."
    (when-let* ((root (locate-dominating-file
                      dir
                      (lambda (d)
                        (let ((markers '(".lsp" ".ccls" "compile_commands.json"
                                         "compile_flags.txt" ".clangd" "tsconfig.json"
                                         "pyrightconfig.json" ".pylintrc" "setup.cfg"
                                         "Cargo.toml" "go.mod" "pom.xml"
                                         "build.gradle")))
                          (and (not (string= (expand-file-name d) (expand-file-name "~/")))
                               (cl-some (lambda (f)
                                          (file-exists-p (expand-file-name f d)))
                                        markers)))))))
      (unless (string= (expand-file-name root) (expand-file-name "~/"))
        (cons 'vc root))))

  (add-to-list 'project-find-functions #'my-project-try-lsp t)

  ;; Auto-discover projects in search paths
  (defun my-project-discover-projects ()
    "Scan project search paths and remember them."
    (dolist (search-path '("~/Code" "~/Projects"))
      (when (file-directory-p search-path)
        (dolist (dir (directory-files search-path t))
          (when (and (file-directory-p dir)
                     (not (member (file-name-nondirectory dir) '("." "..")))
                     (file-exists-p (expand-file-name ".git" dir)))
            (let ((proj (project-current nil dir)))
              (when proj
                (project-remember-project proj))))))))

  (my-project-discover-projects))

(provide 'init-project)

;;; init-project.el ends here
