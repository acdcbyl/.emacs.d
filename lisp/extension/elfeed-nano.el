;;; elfeed-nano.el --- Lightweight nano-style elfeed UI -*- lexical-binding: t -*-

;;; Commentary:
;;
;; A minimal, dependency-light re-implementation of the nano-elfeed look.
;; No nano-theme / relative-date / stripes / s packages needed; it only
;; uses `elfeed' and `nerd-icons' (both already installed).
;;
;; What you get:
;;
;;   * 2-line entry cards in the *elfeed-search* buffer:
;;       [icon] Entry title ............ relative date
;;              feed title (faded, truncated)
;;   * alternating row backgrounds and a highlighted current row,
;;     colors auto-adapted to whatever theme is active (doom-matugen here)
;;   * relative dates: "just now", "12m ago", "3h ago", "Yesterday", ...
;;   * feed icons via nerd-icons (customizable), orange RSS icon when unread;
;;     icons are matched against the feed title AND feed URL, so both
;;     "r/emacs" (title) and "reddit.com" (URL) get the Reddit icon
;;   * `n'/`p' (and arrows) move entry-by-entry; `n'/`p' in the reading
;;     view go to the next/previous article
;;
;; Usage:
;;
;;   (require 'elfeed-nano)
;;   (elfeed-nano-mode)
;;
;; Customize `elfeed-nano-feed-icons' to map feed titles to nerd-icons.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'elfeed)
(require 'nerd-icons)

(defgroup elfeed-nano nil
  "A lightweight nano-style elfeed UI."
  :group 'elfeed)

;; ---------------------------------------------------------------------------
;; Faces (adapted to the active theme at load / theme-change time)
;; ---------------------------------------------------------------------------

(defface elfeed-nano-title-face
  '((t :inherit bold :extend t))
  "Face for the title of unread entries.")

(defface elfeed-nano-title-read-face
  '((t :inherit font-lock-comment-face :extend t))
  "Face for the title of read entries.")

(defface elfeed-nano-subtitle-face
  '((t :inherit font-lock-comment-face :extend t))
  "Face for the feed/subtitle line.")

(defface elfeed-nano-stripe-face
  '((t :extend t))
  "Background of alternating rows.  Colors set from the active theme.")

(defface elfeed-nano-hl-face
  '((t :extend t))
  "Background of the current row.  Colors set from the active theme.")

(defun elfeed-nano--dark-p (color)
  "Return non-nil when COLOR is a dark color."
  (let ((rgb (color-name-to-rgb color)))
    (< (+ (* 0.299 (nth 0 rgb))
          (* 0.587 (nth 1 rgb))
          (* 0.114 (nth 2 rgb)))
       0.5)))

(defun elfeed-nano--update-theme-faces ()
  "Adapt stripe/highlight background colors to the active theme."
  (let ((bg (face-background 'default)))
    (when (and bg (color-defined-p bg))
      (let* ((dark (elfeed-nano--dark-p bg))
             (stripe (if dark (color-lighten-name bg 2)
                       (color-darken-name bg 2)))
             (hl     (if dark (color-lighten-name bg 7)
                       (color-darken-name bg 5))))
        (set-face-attribute 'elfeed-nano-stripe-face nil :background stripe)
        (set-face-attribute 'elfeed-nano-hl-face nil :background hl)))))

(when (boundp 'after-load-theme-hook)
  (add-hook 'after-load-theme-hook #'elfeed-nano--update-theme-faces))

;; ---------------------------------------------------------------------------
;; Icons
;; ---------------------------------------------------------------------------

(defcustom elfeed-nano-feed-icons
  ;; First match wins; regexps are matched against the lowercased
  ;; "feed-title feed-url" string, so both the site name and the domain
  ;; work.  Brand icons come before the generic category rules.
  '(;; --- your own feeds --------------------------------------------
    ("半月谈\\|banyuetan"      . "nf-md-newspaper")
    ("人民日报\\|people-daily" . "nf-md-newspaper")
    ;; --- brands -----------------------------------------------------
    ("reddit"                . "nf-fa-reddit")
    ("youtube"               . "nf-fa-youtube")
    ("twitter\\|\\bx\\.com"   . "nf-fa-twitter")
    ("github"                . "nf-fa-github")
    ("gitlab"                . "nf-fa-gitlab")
    ("bitbucket"             . "nf-fa-bitbucket")
    ("stack ?overflow"       . "nf-fa-terminal")
    ("hacker ?news\\|y[- ]?combinator" . "nf-fa-fire")
    ("telegram"              . "nf-fa-telegram")
    ("weibo\\|微博"         . "nf-fa-weibo")
    ("weixin\\|wechat\\|微信\\|公众号" . "nf-fa-weixin")
    ("zhihu\\|知乎"         . "nf-fa-comments")
    ("bilibili\\|哔哩"      . "nf-fa-play")
    ("medium"                . "nf-fa-medium")
    ("wordpress"             . "nf-fa-wordpress")
    ("blogger"               . "nf-fa-blogger")
    ("dev\\.to\\|devto"   . "nf-fa-dev")
    ("slack"                 . "nf-fa-slack")
    ("discord"               . "nf-fa-discord")
    ("steam"                 . "nf-fa-steam")
    ("spotify"               . "nf-fa-spotify")
    ("podcast\\|simplecast" . "nf-fa-podcast")
    ("soundcloud"            . "nf-fa-soundcloud")
    ("twitch"                . "nf-fa-twitch")
    ("facebook"              . "nf-fa-facebook")
    ("instagram"             . "nf-fa-instagram")
    ("linkedin"              . "nf-fa-linkedin")
    ("pinterest"             . "nf-fa-pinterest")
    ("apple\\|9to5mac\\|macrumors" . "nf-fa-apple")
    ("androidpolice\\|android" . "nf-fa-android")
    ("linux\\|phoronix\\|itsfoss\\|omgubuntu" . "nf-fa-linux")
    ("ubuntu"                . "nf-fa-ubuntu")
    ("docker"                . "nf-fa-docker")
    ("google"                . "nf-fa-google")
    ("tumblr"                . "nf-fa-tumblr")
    ("dropbox"               . "nf-fa-dropbox")
    ("qq\\.com\\|renren"  . "nf-fa-qq")
    ;; --- generic categories -----------------------------------------
    ("arxiv\\|biorxiv\\|sciencedaily\\|nature\\.com\\|plos\\|elife\\|jneuro\\|neuroscience\\|sciencemag\\|phys\\.org"
                               . "nf-fa-flask")
    ("nytimes\\|guardian\\|bbc\\|cnn\\|reuters\\|washingtonpost\\|economist\\|bloomberg\\|wsj\\|forbes\\|ft\\.com\\|npr\\|bleepingcomputer\\|theregister\\|wired\\|arstechnica\\|theverge\\|engadget\\|gizmodo\\|slashdot\\|boingboing\\|techcrunch\\|pcmag\\|zdnet\\|thenextweb"
                               . "nf-fa-newspaper")
    ("emacs\\|irreal\\|emacslife" . "nf-md-emoticon"))
  "Alist of (regexp . nerd-icons name) for feed icons.
Regexps are matched (first match wins) against the lowercased feed
title + feed URL.  Feeds matching nothing get the generic RSS icon.

See `nerd-icons-mdicon' / `nerd-icons-faicon' for available names."
  :type '(alist :key-type (string :tag "Regexp")
                :value-type (string :tag "nerd-icons name"))
  :group 'elfeed-nano)

(defun elfeed-nano--feed-icon-name (feed)
  "Return the nerd-icons name for FEED, or the RSS fallback.
Matching is done against the feed title and URL (see
`elfeed-nano-feed-icons')."
  (let ((haystack (downcase (concat (or (elfeed-feed-title feed) "")
                                    " " (or (elfeed-feed-url feed) "")))))
    (or (cdr (cl-find-if (lambda (pair)
                           (string-match-p (car pair) haystack))
                         elfeed-nano-feed-icons))
        "nf-md-rss")))

(defun elfeed-nano--face-fg (face)
  "Return the foreground color of FACE, or nil when unspecified."
  (let ((fg (face-foreground face)))
    (and fg (not (equal fg "unspecified-fg")) fg)))

(defun elfeed-nano--icon (feed unread &optional background)
  "Return a colored icon string for FEED, bright when UNREAD.
BACKGROUND is an optional stripe background color to merge in."
  (let* ((name (elfeed-nano--feed-icon-name feed))
         (accessor (cond ((string-prefix-p "nf-fa-" name)
                          #'nerd-icons-faicon)
                         ((string-prefix-p "nf-oct-" name)
                          #'nerd-icons-octicon)
                         ((string-prefix-p "nf-cod-" name)
                          #'nerd-icons-codicon)
                         (t #'nerd-icons-mdicon)))
         (glyph (funcall accessor name))
         (face (plist-get (text-properties-at 0 glyph) 'face))
         (face (if (listp face) face (list face)))
         (fg (cond ((not unread) (elfeed-nano--face-fg 'font-lock-comment-face))
                   ((equal name "nf-md-rss") (elfeed-nano--face-fg 'warning))
                   (t (elfeed-nano--face-fg 'default))))
         (spec (append (list :foreground (or fg "gray")) face)))
    (when background
      (setq spec (plist-put spec :background background)))
    ;; Keep BOTH `face' and `font-lock-face' in sync: nerd-icons relies on
    ;; the :family here to pick the Nerd Font, which is otherwise lost when
    ;; we merge the icon into the entry line.
    (propertize glyph 'face spec 'font-lock-face spec)))

;; ---------------------------------------------------------------------------
;; Entry rendering
;; ---------------------------------------------------------------------------

(defun elfeed-nano--relative-date (time)
  "Return a short relative date string for TIME (epoch seconds)."
  (let* ((diff (- (float-time) time))
         (secs (abs diff)))
    (cond ((< secs 60) "just now")
          ((< secs 3600) (format "%dm ago" (max 1 (floor (/ secs 60)))))
          ((< secs 86400) (format "%dh ago" (max 1 (floor (/ secs 3600)))))
          ((< secs 172800) "yesterday")
          ((< secs (* 7 86400)) (format "%dd ago" (floor (/ secs 86400))))
          ((< secs (* 30 86400)) (format "%dw ago" (floor (/ secs (* 7 86400)))))
          (t (format-time-string "%Y-%m-%d" time)))))

(defun elfeed-nano--truncate (str width)
  "Return STR truncated to WIDTH columns, with an ellipsis when cut."
  (if (> (string-width str) width)
      (concat (truncate-string-to-width str (max 1 (1- width))) "…")
    str))

(defun elfeed-nano-search-print-entry (entry)
  "Print ENTRY as a two-line card (feed icon / title / date / feed name)."
  (let* ((unread (memq 'unread (elfeed-entry-tags entry)))
         (title (or (elfeed-meta entry :title)
                    (elfeed-entry-title entry) ""))
         (feed (elfeed-entry-feed entry))
         (feed-title (if feed
                         (or (elfeed-meta feed :title)
                             (elfeed-feed-title feed))
                       ""))
         (date (elfeed-nano--relative-date (elfeed-entry-date entry)))
         (icon (elfeed-nano--icon feed unread))
         (icon-w (1+ (string-width icon)))       ; icon + trailing space
         (pad 2)
         (width (max 20 (- (window-width) icon-w pad)))
         (date-w (+ (string-width date) pad))
         (title-face (if unread 'elfeed-nano-title-face
                       'elfeed-nano-title-read-face))
         ;; Alternate row backgrounds.  Stateless: point is at the start of
         ;; the entry being printed, and each previous entry took 2 lines.
         (stripe (when (zerop (mod (/ (count-lines (point-min) (point)) 2) 2))
                   'elfeed-nano-stripe-face))
         (line-face (delq nil (list title-face stripe)))
         (stripe-bg (and stripe (face-background 'elfeed-nano-stripe-face)))
         (title-str (elfeed-nano--truncate title width))
         (subtitle (elfeed-nano--truncate feed-title width)))
    ;; The icon is inserted with its own face (Nerd Font family) so the
    ;; glyph still renders; the rest of the line carries the title/stripe
    ;; faces.
    (insert (elfeed-nano--icon feed unread stripe-bg)
            (propertize " " 'face line-face)
            (propertize title-str 'face line-face)
            (propertize " " 'face line-face
                        'display `(space :align-to (- right ,date-w)))
            (propertize date 'face line-face)
            "\n"
            (propertize (concat (make-string icon-w ?\s) subtitle)
                        'face (delq nil
                                    (list 'elfeed-nano-subtitle-face stripe))))))

;; ---------------------------------------------------------------------------
;; Modes & keybindings
;; ---------------------------------------------------------------------------

(defun elfeed-nano-search-mode ()
  "Set up the *elfeed-search* buffer for the nano look."
  (elfeed-nano--update-theme-faces)
  (setq-local truncate-lines t)
  (setq-local cursor-type nil)
  (face-remap-add-relative 'hl-line :inherit 'elfeed-nano-hl-face))

(defun elfeed-nano-show-mode ()
  "Set up the *elfeed-show* buffer."
  (visual-line-mode 1)
  (setq-local truncate-lines nil)
  (setq-local shr-width 80))

(defun elfeed-nano-next-entry ()
  "Move point to the next entry in the search buffer."
  (interactive)
  (text-property-search-forward 'elfeed-entry t))

(defun elfeed-nano-prev-entry ()
  "Move point to the previous entry in the search buffer."
  (interactive)
  (text-property-search-backward 'elfeed-entry t))

(defun elfeed-nano-show-next ()
  "Show the next entry in the search buffer."
  (interactive)
  (funcall elfeed-show-entry-delete)
  (with-current-buffer (elfeed-search-buffer)
    (when (elfeed-search--remain-on-entry-p 'show) (elfeed-nano-next-entry))
    (call-interactively #'elfeed-search-show-entry)))

(defun elfeed-nano-show-prev ()
  "Show the previous entry in the search buffer."
  (interactive)
  (funcall elfeed-show-entry-delete)
  (with-current-buffer (elfeed-search-buffer)
    (when (elfeed-search--remain-on-entry-p 'show) (elfeed-nano-next-entry))
    (elfeed-nano-prev-entry)
    (elfeed-nano-prev-entry)
    (call-interactively #'elfeed-search-show-entry)))

;;;###autoload
(defun elfeed-nano--entry-start (n)
  "Return the buffer position of the first line of entry N (0-based)."
  (save-excursion
    (goto-char (point-min))
    (forward-line (* 2 n))
    (point)))

(defun elfeed-nano--update-line (&optional n)
  "Redraw entry N (two lines), overriding `elfeed-search--update-line'."
  (let ((inhibit-read-only t))
    (save-excursion
      (when n
        (goto-char (elfeed-nano--entry-start (1- n))))
      (when-let* ((entry (elfeed-search-selected :ignore-region)))
        (elfeed-search--remove-marked-overlay entry)
        ;; delete the whole two-line entry, not just one line
        (let ((beg (pos-bol)))
          (delete-region beg (min (point-max) (pos-bol 3))))
        (elfeed-search--print-entry entry)
        (insert "\n")
        (when (memq entry elfeed-search--marked)
          (elfeed-search--make-marked-overlay entry))))))

(defun elfeed-nano--remove-marked-overlay (entry)
  "Remove the mark overlay of ENTRY, two-line aware."
  (when-let* ((n (cl-position entry elfeed-search-entries)))
    (save-excursion
      (goto-char (elfeed-nano--entry-start n))
      (remove-overlays (pos-bol) (pos-bol 3)
                       'category 'elfeed-search-marked))))

(defun elfeed-nano--make-marked-overlay (entry)
  "Add the mark overlay over ENTRY, two-line aware."
  (when-let* ((n (cl-position entry elfeed-search-entries)))
    (save-excursion
      (goto-char (elfeed-nano--entry-start n))
      (overlay-put (make-overlay (pos-bol) (pos-bol 3))
                   'category 'elfeed-search-marked))))

(defun elfeed-nano-mode ()
  "Enable the lightweight nano-style elfeed UI."
  (interactive)
  (setq elfeed-search-print-entry-function #'elfeed-nano-search-print-entry)
  (add-hook 'elfeed-search-mode-hook #'elfeed-nano-search-mode)
  (add-hook 'elfeed-show-mode-hook #'elfeed-nano-show-mode)
  ;; Date separator lines don't fit the card layout.
  (remove-hook 'elfeed-search-update-hook #'elfeed-search-add-separators)
  ;; elfeed's single-entry update helpers are line-based; our entries are
  ;; two lines tall, so route them through two-line aware versions.
  (unless (advice-member-p #'elfeed-nano--update-line
                           'elfeed-search--update-line)
    (advice-add 'elfeed-search--update-line :override
                #'elfeed-nano--update-line))
  (unless (advice-member-p #'elfeed-nano--remove-marked-overlay
                           'elfeed-search--remove-marked-overlay)
    (advice-add 'elfeed-search--remove-marked-overlay :override
                #'elfeed-nano--remove-marked-overlay))
  (unless (advice-member-p #'elfeed-nano--make-marked-overlay
                           'elfeed-search--make-marked-overlay)
    (advice-add 'elfeed-search--make-marked-overlay :override
                #'elfeed-nano--make-marked-overlay))
  (elfeed-nano--update-theme-faces)
  (with-eval-after-load 'elfeed-search
    (keymap-set elfeed-search-mode-map "n" #'elfeed-nano-next-entry)
    (keymap-set elfeed-search-mode-map "p" #'elfeed-nano-prev-entry)
    (keymap-set elfeed-search-mode-map "<down>" #'elfeed-nano-next-entry)
    (keymap-set elfeed-search-mode-map "<up>" #'elfeed-nano-prev-entry))
  (with-eval-after-load 'elfeed-show
    (keymap-set elfeed-show-mode-map "n" #'elfeed-nano-show-next)
    (keymap-set elfeed-show-mode-map "p" #'elfeed-nano-show-prev)))

(provide 'elfeed-nano)
;;; elfeed-nano.el ends here
