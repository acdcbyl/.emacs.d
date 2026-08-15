;;; init-dired.el --- Some tweaks for dired -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;;Dired beautification and enhancement
(use-package
  dired
  :config
  ;; Guess a default target directory
  (setq dired-dwim-target t)

  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  ;; Show directory first
  (setq dired-listing-switches "-alh --group-directories-first")
  )

;; Make dired colorful
(use-package
  diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

;; Show git info in dired
(use-package
  dired-git-info
  :ensure t
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

;; Extra Dired functionality
(use-package
  dired-aux
  :ensure nil
  :after dired
  :config
  (with-no-warnings
    (defvar dired-dotfiles-show t)
    (defun dired-dotfiles-toggle (&rest _)
      "Show/hide dotfiles."
      (interactive)
      (if (not dired-dotfiles-show)
          (revert-buffer)
        (dired-mark-files-regexp "^\\.")
        (dired-do-kill-lines))
      (setq-local dired-dotfiles-show (not dired-dotfiles-show)))

    (advice-add 'dired-do-print :override #'dired-dotfiles-toggle))
  :custom
  (dired-vc-rename-file t)
  (dired-do-revert-buffer t)
  (dired-isearch-filenames 'dwim)
  (dired-create-destination-dirs 'ask))

(use-package
  dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  (dired-omit-files (rx string-start
                        (or ".DS_Store"
                            ".cache"
                            ".vscode"
                            ".ccls-cache" ".clangd")
                        string-end))
  (dired-guess-shell-alist-user `((,(rx "."
                                        (or
                                         ;; Videos
                                         "mp4" "avi" "mkv" "flv" "ogv" "ogg" "mov"
                                         ;; Music
                                         "wav" "mp3" "flac"
                                         ;; Images
                                         "jpg" "jpeg" "png" "gif" "xpm" "svg" "bmp"
                                         ;; Docs
                                         "pdf" "md" "djvu" "ps" "eps" "doc" "docx" "xls" "xlsx" "ppt" "pptx")
                                        string-end)
                                   ,(pcase system-type
                                      ('gnu/linux "xdg-open")
                                      ('darwin "open")
                                      ('windows-nt "start")
                                      (_ ""))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   File operations (bound in init-evil.el: SPC f C / D / y / R)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun +copy-current-file ()
  "Copy the current file to a new location."
  (interactive)
  (if-let* ((path (buffer-file-name)))
      (let* ((new-path (read-file-name "Copy current file to: "))
             (new-dir (file-name-directory new-path)))
        (unless (file-exists-p new-dir)
          (make-directory new-dir t))
        (copy-file path new-path 1)
        (message "Copied %s to %s" path new-path))
    (user-error "Buffer is not visiting a file")))

(defun +delete-current-file ()
  "Delete the current file after confirmation, then kill its buffer."
  (interactive)
  (if-let* ((path (buffer-file-name)))
      (when (y-or-n-p (format "Delete %s? " (file-name-nondirectory path)))
        (delete-file path)
        (kill-current-buffer)
        (message "Deleted %s" path))
    (user-error "Buffer is not visiting a file")))

(defun +copy-current-filename ()
  "Copy the current file's absolute path to the kill ring."
  (interactive)
  (if-let* ((path (buffer-file-name)))
      (progn (kill-new path)
             (message "Copied path: %s" path))
    (user-error "Buffer is not visiting a file")))

(defun +rename-current-file ()
  "Rename the current file and keep visiting it under the new name."
  (interactive)
  (if-let* ((path (buffer-file-name)))
      (let* ((dir (file-name-directory path))
             (new-path (read-file-name "Rename to: " dir
                                       (file-name-nondirectory path))))
        (rename-file path new-path 1)
        (set-visited-file-name new-path t t)
        (message "Renamed to %s" new-path))
    (user-error "Buffer is not visiting a file")))

(provide 'init-dired)

;;; init-dired.el ends here.
