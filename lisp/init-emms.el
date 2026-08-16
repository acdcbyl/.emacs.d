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

(defun +emms-cover-cache-directory ()
  "Return the directory used for cached embedded artwork."
  (expand-file-name "covers/" emms-directory))

(defvar +emms-cover-max-size 1024
  "Maximum width/height in pixels for cached extracted covers.
Embedded artwork larger than this is scaled down before caching, keeping
the cache small and fast for Emacs to decode and display.")

(defun +emms-track-cover-cache-file (track)
  "Return the cached cover filename for TRACK."
  (expand-file-name
   (concat (md5 (expand-file-name track)) ".jpg")
   (+emms-cover-cache-directory)))

(defun +emms-track-thumbnail-cache-file (track size)
  "Return the cached thumbnail filename for TRACK at SIZE."
  (expand-file-name
   (format "%s-%s.png" (md5 (expand-file-name track)) size)
   (+emms-cover-cache-directory)))

(defun +emms-cover-size-pixels (size)
  "Return the pixel size configured for an EMMS cover SIZE."
  (pcase size
    ('small emms-browser-thumbnail-small-size)
    ('medium emms-browser-thumbnail-medium-size)
    ('large emms-browser-thumbnail-large-size)
    (_ emms-browser-thumbnail-medium-size)))

(defun +emms-extract-embedded-cover (track)
  "Return TRACK's embedded artwork as a cached, scaled cover file.

The artwork is extracted with ffmpeg, scaled down to at most
`+emms-cover-max-size' pixels, and stored as a JPEG so the cache stays small
and quick for Emacs to decode and display.  The cache is keyed by TRACK
instead of by directory, so different files in the same directory may keep
different embedded covers."
  (when-let* ((ffmpeg (executable-find "ffmpeg"))
              ((file-readable-p track)))
    (let* ((cover (+emms-track-cover-cache-file track))
           (temporary-cover (concat cover ".tmp.jpg")))
      (when (or (not (file-readable-p cover))
                (file-newer-than-file-p track cover))
        (make-directory (file-name-directory cover) t)
        (when (file-exists-p temporary-cover)
          (delete-file temporary-cover))
        (if (zerop
             (call-process
              ffmpeg nil nil nil "-nostdin" "-v" "error" "-y"
              "-i" track "-map" "0:v:0" "-frames:v" "1" "-update" "1"
              "-vf" (format "scale='min(%d,iw)':'min(%d,ih)':force_original_aspect_ratio=decrease"
                            +emms-cover-max-size +emms-cover-max-size)
              "-q:v" "3" temporary-cover))
            (rename-file temporary-cover cover t)
          (when (file-exists-p temporary-cover)
            (delete-file temporary-cover))))
      (and (file-readable-p cover) cover))))

(defun +emms-resize-cover (track size)
  "Return a cached thumbnail for TRACK's embedded cover at SIZE."
  (if (eq size 'large)
      (+emms-extract-embedded-cover track)
    (when-let* ((source-cover (+emms-extract-embedded-cover track)))
      (if-let* ((convert (or (executable-find "magick")
                             (executable-find "convert"))))
          (let* ((thumbnail (+emms-track-thumbnail-cache-file track size))
                 (temporary-thumbnail (concat thumbnail ".tmp.png"))
                 (pixels (+emms-cover-size-pixels size)))
            (when (or (not (file-readable-p thumbnail))
                      (file-newer-than-file-p source-cover thumbnail))
              (when (file-exists-p temporary-thumbnail)
                (delete-file temporary-thumbnail))
              (if (zerop
                   (if (string-equal (file-name-base convert) "magick")
                       (call-process
                        convert nil nil nil source-cover "-resize"
                        (format "%sx%s" pixels pixels) temporary-thumbnail)
                     (call-process
                      convert nil nil nil source-cover "-resize"
                      (format "%sx%s" pixels pixels) temporary-thumbnail)))
                  (rename-file temporary-thumbnail thumbnail t)
                (when (file-exists-p temporary-thumbnail)
                  (delete-file temporary-thumbnail))))
            (or (and (file-readable-p thumbnail) thumbnail)
                source-cover))
        source-cover))))

(defun +emms-extract-embedded-covers ()
  "Extract missing large covers from embedded artwork."
  (interactive)
  (require 'emms-browser)
  (let (tracks-without-cover)
    (dolist (track
             (+emms-source-file-directory-tree-fd
              emms-source-file-default-directory (emms-source-file-regex)))
      (unless (+emms-extract-embedded-cover track)
        (push track tracks-without-cover)))
    (when tracks-without-cover
      (message "EMMS: %d tracks had no readable embedded cover"
               (length tracks-without-cover))))
  (when (hash-table-p emms-browser--cache-hash)
    (emms-browser-clear-cache-hash))
  (when (buffer-live-p emms-browser-buffer)
    (kill-buffer emms-browser-buffer)))

(defun +emms-directory-first-track (directory)
  "Return the first playable track directly below DIRECTORY."
  (seq-find
   (lambda (file)
     (and (file-regular-p file)
          (string-match-p (emms-source-file-regex) file)))
   (directory-files directory t directory-files-no-dot-files-regexp)))

(defun +emms-track-for-cover-path (path)
  "Return a playable track for cover PATH.

PATH may be a track file or a directory, depending on the caller."
  (cond
   ((and (stringp path)
         (file-regular-p path)
         (string-match-p (emms-source-file-regex) path))
    path)
   ((and (stringp path)
         (file-directory-p path))
    (+emms-directory-first-track path))))

(defun +emms-browser-cover (directory size)
  "Return an EMMS SIZE cover from DIRECTORY, extracting artwork on demand.

EMMS browser asks for covers by directory.  For large covers, use the first
track in DIRECTORY as that directory's representative cover, but keep the cache
file keyed by the track itself."
  (when-let* ((track (+emms-track-for-cover-path directory)))
    (+emms-resize-cover track size)))

(defun +emms-browser-get-cover-from-track-path (oldfun path &optional size)
  "Use embedded artwork from PATH before falling back to OLDFUN."
  (or (when-let* ((track (+emms-track-for-cover-path path)))
        (+emms-resize-cover track (or size 'medium)))
      (funcall oldfun path size)))

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
    "mk"  'emms-volume-mode-plus
    "m+"  'emms-volume-mode-plus
    "mj"  'emms-volume-mode-minus
    "m-"  'emms-volume-mode-minus
    "mu"  (list :wk (format "%s emms ui" (nerd-icons-mdicon "nf-md-music_note")))
    "muu" 'emms-ui
    "mun" 'emms-ui-now-playing
    "mul" 'emms-ui-list
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

  ;; EMMS's `emms-time-less-p' assumes the legacy (HIGH LOW) timestamp
  ;; format.  Modern Emacs (29+) returns file mtimes from
  ;; `file-attributes' as (TICKS . HZ) (e.g. (1729675943458041424 .
  ;; 1000000000)), which made `emms-time-less-p' call (car 1000000000)
  ;; on the HZ slot when comparing two equal timestamps, signalling
  ;; "Wrong type argument: listp, 1000000000" whenever `emms-cache-sync'
  ;; runs (e.g. from `emms-ui-list').  Use Emacs's built-in `time-less-p'
  ;; instead, which understands every timestamp representation.
  (advice-add 'emms-time-less-p :override #'time-less-p)

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
  (advice-add 'emms-browser-get-cover-from-path
              :around #'+emms-browser-get-cover-from-track-path)
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

;; Evil bindings for emms-ui.  evil-collection does not cover these modes, so
;; default them to normal state and map left/right (and row movement) onto h/l
;; (and j/k) as in the original keymap's <left>/<right>/<up>/<down>.
(defun +emms-ui-albums-next-row (&optional count)
  "Move COUNT rows down in the EMMS album grid."
  (interactive "p")
  (emms-ui-albums--move-row count))

(defun +emms-ui-albums-previous-row (&optional count)
  "Move COUNT rows up in the EMMS album grid."
  (interactive "p")
  (emms-ui-albums--move-row (- count)))

(with-eval-after-load 'emms-ui
  (evil-set-initial-state 'emms-ui-albums-mode 'normal)
  (evil-set-initial-state 'emms-ui-list-mode 'normal)
  (evil-set-initial-state 'emms-ui-now-playing-mode 'normal)

  (general-define-key
   :states '(normal)
   :keymaps 'emms-ui-albums-mode-map
   "h" #'emms-ui-albums-previous
   "l" #'emms-ui-albums-next
   "L" #'emms-ui-list
   "j" #'+emms-ui-albums-next-row
   "k" #'+emms-ui-albums-previous-row)

  (general-define-key
   :states '(normal)
   :keymaps 'emms-ui-now-playing-mode-map
   "h" #'emms-seek-backward
   "l" #'emms-seek-forward))
(provide 'init-emms)
;;; init-emms.el ends here
