;;; init-emms.el --- Bring music/video player for emacs -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for EMMS (Emacs Multimedia System), adapted for aiser's Emacs setup
;; (general.el + evil + package.el + nerd-icons + tabspaces).

;;; Code:

(require 'nerd-icons)

(defun +emms-source-file-directory-tree-fd (dir regex)
  "Return files below DIR whose absolute names match REGEX."
  (let ((directory (expand-file-name dir)))
    (when (file-directory-p directory)
      (if-let* ((fd (executable-find "fd")))
          (with-temp-buffer
            (let ((status
                   (call-process fd nil t nil
                                 "--type" "f" "--absolute-path" "--print0"
                                 "." directory)))
              (unless (zerop status)
                (error "fd failed: %s" (string-trim (buffer-string))))
              (seq-filter
               (lambda (file) (string-match-p regex file))
               (split-string (buffer-string) "\0" t))))
        (directory-files-recursively directory regex)))))

(defun +emms-extract-embedded-cover (track)
  "Return TRACK's embedded artwork as a large cover file."
  (let ((cover (expand-file-name "cover_large.png"
                                 (file-name-directory track))))
    (unless (file-readable-p cover)
      (unless (zerop
               (call-process
                (executable-find "ffmpeg") nil nil nil "-nostdin" "-v" "error" "-y"
                "-i" track "-map" "0:v:0" "-frames:v" "1" "-update" "1" cover))
        (when (file-exists-p cover) (delete-file cover))))
    (and (file-readable-p cover) cover)))

(defun +emms-extract-embedded-covers ()
  "Extract missing large covers from embedded artwork."
  (interactive)
  (require 'emms-browser)
  (let (albums)
    (dolist (track
             (+emms-source-file-directory-tree-fd
              emms-source-file-default-directory (emms-source-file-regex)))
      (unless (member (file-name-directory track) albums)
        (push (file-name-directory track) albums)
        (+emms-extract-embedded-cover track))))
  (when (hash-table-p emms-browser--cache-hash)
    (emms-browser-clear-cache-hash))
  (when (buffer-live-p emms-browser-buffer)
    (kill-buffer emms-browser-buffer)))

(defun +emms-browser-cover (directory size)
  "Return an EMMS SIZE cover from DIRECTORY, extracting large artwork on demand."
  (if (eq size 'large)
      (let ((cover (expand-file-name "cover_large.png" directory)))
        (or (and (file-readable-p cover) cover)
            (when-let* ((track (car (directory-files
                                     directory t (emms-source-file-regex)))))
              (+emms-extract-embedded-cover track))))
    (emms-browser-cache-thumbnail-async directory size)))

(defun +emms-lyrics-find-with-info-lyric (file)
  "Find external lyric FILE, falling back to the current track's tag."
  (or (emms-lyrics-find-lyric file)
      (when-let* ((track (emms-playlist-current-selected-track))
                  (_ (or (emms-track-get track 'info-lyrics)
                         (emms-info-exiftool track)))
                  (lyrics (emms-track-get track 'info-lyrics))
                  (cache-file (expand-file-name
                               (concat (md5 (emms-track-name track)) ".lrc")
                               (expand-file-name "lyrics/" emms-directory)))
                  ((stringp lyrics))
                  ((not (string-empty-p lyrics))))
        (when (or (not (file-exists-p cache-file))
                  (file-newer-than-file-p (emms-track-name track)
                                          cache-file))
          (make-directory (file-name-directory cache-file) t)
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file cache-file
              (insert lyrics))))
        cache-file)))

(autoload 'emms-playlist-mode-go "emms-playlist-mode" nil t)
(autoload 'emms-tag-editor-edit "emms-tag-editor" nil t)
(autoload 'emms-lyrics-visit-lyric "emms-lyrics" nil t)
(autoload 'emms-info-exiftool "emms-info-exiftool")
(autoload 'emms-last-played-update-current "emms-last-played")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EMMS Core Package
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emms
  :ensure t
  :commands (emms-smart-browse
             emms-browser
             emms-add-url
             emms-add-file
             emms-add-directory
             emms-add-find
             emms-play-file
             emms-play-directory
             emms-playlist-mode-go)
  :general
  (aiser/leader-def
    "m"   (list :wk (format "%s music" (nerd-icons-mdicon "nf-md-music_note")))
    "ms"  'emms-smart-browse
    "mb"  'emms-browser
    "ml"  'emms-playlist-mode-go
    "mp"  'emms-pause
    "mS"  'emms-stop
    "mq"  'emms-stop
    "m]"  'emms-next
    "mn"  'emms-next
    "m["  'emms-previous
    "mP"  'emms-previous
    "mu"  'emms-player-mpd-connect
    "mww" 'emms-lyrics
    "mk"  'emms-volume-mode-plus
    "m+"  'emms-volume-mode-minus
    "mj"  'emms-volume-mode-plus
    "m-"  'emms-volume-mode-minus
    "mc"  'aiser/emms-cleanup-urls
    "ma"  (list :wk (format "%s add" (nerd-icons-mdicon "nf-md-playlist_plus")))
    "maf" 'emms-add-file
    "mad" 'emms-add-directory
    "mau" 'emms-add-url
    "mas" 'emms-add-find)

  :init
  (setq emms-directory (no-littering-expand-var-file-name "emms/")
        emms-cache-file (no-littering-expand-var-file-name "emms/cache")
        emms-history-file (no-littering-expand-var-file-name "emms/history")
        emms-source-file-default-directory "~/Music/"
        emms-player-list '(emms-player-mpv)
        emms-player-mpv-parameters '("--quiet" "--no-video" "--force-window=no"))
  (make-directory emms-directory t)

  :config
  (require 'emms-player-mpv)
  (require 'emms-cache)
  (require 'emms-history)
  (require 'emms-compat)

  (add-to-list 'emms-track-initialize-functions
               #'emms-info-initialize-track)
  (add-hook 'emms-player-started-hook
            #'emms-last-played-update-current)

  (with-eval-after-load 'emms-info-exiftool
    (add-to-list 'emms-info-exiftool-field-map '(info-lyrics . Lyrics)))

  (setq emms-browser-covers #'+emms-browser-cover
        emms-browser-thumbnail-small-size 64
        emms-browser-thumbnail-medium-size 128
        emms-info-functions '(emms-info-exiftool)
        emms-track-description-function #'emms-info-track-description
        emms-lyrics-find-lyric-function #'+emms-lyrics-find-with-info-lyric
        emms-lyrics-scroll-p nil
        emms-source-file-directory-tree-function #'+emms-source-file-directory-tree-fd
        emms-playlist-buffer-name "*Music*"
        emms-playlist-mode-center-when-go t
        emms-info-asynchronously t
        emms-info-auto-update t
        emms-volume-change-amount 5
        emms-volume-change-function #'emms-volume-mpv-change
        emms-show-format "♪ %s")

  (emms-cache 1)
  (setopt emms-player-mpv-update-metadata t))

;; Keymaps for quitting EMMS windows with 'q'
(with-eval-after-load 'emms-browser
  (general-define-key
   :states '(normal)
   :keymaps 'emms-browser-mode-map
   "q" #'quit-window))

(with-eval-after-load 'emms-playlist-mode
  (general-define-key
   :states '(normal)
   :keymaps 'emms-playlist-mode-map
   "q" #'quit-window))

(use-package emms-ui
  :vc (:url "https://github.com/roife/emms-ui" :rev :newest)
  :after emms
  :commands (emms-ui emms-ui-albums emms-ui-list
                     emms-ui-now-playing))
(provide 'init-emms)
;;; init-emms.el ends here
