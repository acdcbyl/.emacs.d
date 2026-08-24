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
;;   * feed icons with brand/category colors via nerd-icons (customizable);
;;     icons are matched against the feed title AND feed URL, so both
;;     "r/emacs" (title) and "reddit.com" (URL) get the Reddit icon (orange)
;;   * `n'/`p' (and arrows) move entry-by-entry; `n'/`p' in the reading
;;     view go to the next/previous article
;;
;; Usage:
;;
;;   (require 'elfeed-nano)
;;   (elfeed-nano-mode)
;;
;; Customize `elfeed-nano-feed-icons' to map feed titles to nerd-icons and colors.

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
  (when-let* ((rgb (color-name-to-rgb color)))
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
  ;; Format: (REGEXP ICON-NAME &optional FACE)
  '(;; --- your own feeds --------------------------------------------
    ("半月谈\\|banyuetan"      "nf-md-newspaper" nerd-icons-red)
    ("人民日报\\|people-daily" "nf-md-newspaper" nerd-icons-red)
    ("新华社\\|xinhua"         "nf-md-newspaper" nerd-icons-red)
    ;; --- brands -----------------------------------------------------
    ("reddit"                "nf-fa-reddit"    nerd-icons-orange)
    ("youtube"               "nf-fa-youtube"   nerd-icons-red)
    ("twitter\\|\\bx\\.com"   "nf-fa-twitter"   nerd-icons-blue)
    ("github"                "nf-fa-github"    nerd-icons-purple)
    ("gitlab"                "nf-fa-gitlab"    nerd-icons-orange)
    ("bitbucket"             "nf-fa-bitbucket" nerd-icons-blue)
    ("stack ?overflow"       "nf-fa-terminal"  nerd-icons-orange)
    ("hacker ?news\\|y[- ]?combinator" "nf-fa-fire" nerd-icons-orange)
    ("telegram"              "nf-fa-telegram"  nerd-icons-cyan)
    ("weibo\\|微博"         "nf-fa-weibo"     nerd-icons-red)
    ("weixin\\|wechat\\|微信\\|公众号" "nf-fa-weixin" nerd-icons-green)
    ("zhihu\\|知乎"         "nf-fa-comments"  nerd-icons-blue)
    ("bilibili\\|哔哩"      "nf-fa-play"      nerd-icons-pink)
    ("medium"                "nf-fa-medium"    nerd-icons-green)
    ("wordpress"             "nf-fa-wordpress" nerd-icons-blue)
    ("blogger"               "nf-fa-blogger"   nerd-icons-orange)
    ("dev\\.to\\|devto"   "nf-fa-dev"       nerd-icons-silver)
    ("slack"                 "nf-fa-slack"     nerd-icons-purple)
    ("discord"               "nf-fa-discord"   nerd-icons-purple)
    ("steam"                 "nf-fa-steam"     nerd-icons-blue)
    ("spotify"               "nf-fa-spotify"   nerd-icons-green)
    ("podcast\\|simplecast" "nf-fa-podcast"   nerd-icons-purple)
    ("soundcloud"            "nf-fa-soundcloud" nerd-icons-orange)
    ("twitch"                "nf-fa-twitch"    nerd-icons-purple)
    ("facebook"              "nf-fa-facebook"  nerd-icons-blue)
    ("instagram"             "nf-fa-instagram" nerd-icons-pink)
    ("linkedin"              "nf-fa-linkedin"  nerd-icons-blue)
    ("pinterest"             "nf-fa-pinterest" nerd-icons-red)
    ("apple\\|9to5mac\\|macrumors" "nf-fa-apple" nerd-icons-silver)
    ("androidpolice\\|android" "nf-fa-android" nerd-icons-green)
    ("linux\\|phoronix\\|itsfoss\\|omgubuntu" "nf-fa-linux" nerd-icons-yellow)
    ("ubuntu"                "nf-fa-ubuntu"    nerd-icons-orange)
    ("docker"                "nf-fa-docker"    nerd-icons-blue)
    ("google"                "nf-fa-google"    nerd-icons-blue)
    ("tumblr"                "nf-fa-tumblr"    nerd-icons-blue)
    ("dropbox"               "nf-fa-dropbox"   nerd-icons-blue)
    ("qq\\.com\\|renren"  "nf-fa-qq"        nerd-icons-cyan)
    ("v2ex"                  "nf-fa-comments"  nerd-icons-silver)
    ("sspai\\|少数派"       "nf-fa-bolt"      nerd-icons-red)
    ("juejin\\|掘金"         "nf-fa-diamond"   nerd-icons-blue)
    ("rust"                  "nf-dev-rust"     nerd-icons-orange)
    ("python"                "nf-dev-python"   nerd-icons-blue)
    ("golang\\|\\bgo\\b"     "nf-dev-go"       nerd-icons-cyan)
    ;; --- generic categories -----------------------------------------
    ("arxiv\\|biorxiv\\|sciencedaily\\|nature\\.com\\|plos\\|elife\\|jneuro\\|neuroscience\\|sciencemag\\|phys\\.org"
                             "nf-fa-flask"     nerd-icons-cyan)
    ("nytimes\\|guardian\\|bbc\\|cnn\\|reuters\\|washingtonpost\\|economist\\|bloomberg\\|wsj\\|forbes\\|ft\\.com\\|npr\\|bleepingcomputer\\|theregister\\|wired\\|arstechnica\\|theverge\\|engadget\\|gizmodo\\|slashdot\\|boingboing\\|techcrunch\\|pcmag\\|zdnet\\|thenextweb"
                             "nf-fa-newspaper" nerd-icons-yellow)
    ("emacs\\|irreal\\|emacslife" "nf-md-emoticon" nerd-icons-purple))
  "Alist of (REGEXP ICON-NAME &optional FACE) for feed icons.
Regexps are matched (first match wins) against the lowercased feed
title + feed URL.  Feeds matching nothing get the generic RSS icon with
`nerd-icons-orange'.

FACE can be a face symbol (e.g. `nerd-icons-red') or a hex color string.
Legacy format (REGEXP . ICON-NAME) is also supported."
  :type '(repeat (list :tag "Rule"
                       (string :tag "Regexp")
                       (string :tag "nerd-icons name")
                       (choice :tag "Face or Color"
                               (const :tag "None" nil)
                               (face :tag "Face")
                               (color :tag "Color string"))))
  :group 'elfeed-nano)

(defcustom elfeed-nano-color-read-icons nil
  "When non-nil, preserve icon colors even for read entries.
When nil (default, nano style), icons for read entries are dimmed using
`font-lock-comment-face'."
  :type 'boolean
  :group 'elfeed-nano)

(defun elfeed-nano--feed-icon-info (feed)
  "Return (ICON-NAME . FACE) for FEED based on `elfeed-nano-feed-icons'.
FEED can be an `elfeed-feed' struct or a string."
  (let* ((haystack (downcase
                    (if (stringp feed)
                        feed
                      (concat (or (elfeed-feed-title feed) "")
                              " " (or (elfeed-feed-url feed) "")))))
         (entry (cl-find-if (lambda (item)
                              (string-match-p (car item) haystack))
                            elfeed-nano-feed-icons)))
    (cond
     ((null entry)
      (cons "nf-md-rss" 'nerd-icons-orange))
     ;; Plist format: ("regexp" :icon "nf-fa-xxx" :face nerd-icons-xxx)
     ((and (listp (cdr entry)) (plist-member (cdr entry) :icon))
      (cons (plist-get (cdr entry) :icon)
            (plist-get (cdr entry) :face)))
     ;; List format: ("regexp" "nf-fa-xxx" [face])
     ((and (listp (cdr entry)) (stringp (cadr entry)))
      (cons (cadr entry) (nth 2 entry)))
     ;; Dotted pair with list: ("regexp" . ("nf-fa-xxx" face))
     ((and (consp (cdr entry)) (stringp (car (cdr entry))))
      (cons (car (cdr entry)) (cadr (cdr entry))))
     ;; Classic dotted pair: ("regexp" . "nf-fa-xxx")
     ((stringp (cdr entry))
      (cons (cdr entry) nil))
     (t
      (cons "nf-md-rss" 'nerd-icons-orange)))))

(defun elfeed-nano--feed-icon-name (feed)
  "Return the nerd-icons name for FEED, or the RSS fallback."
  (car (elfeed-nano--feed-icon-info feed)))

(defun elfeed-nano--face-fg (face)
  "Return the foreground color of FACE (a face symbol or color string), or nil."
  (cond
   ((null face) nil)
   ((and (stringp face) (color-defined-p face)) face)
   ((facep face)
    (let ((fg (face-foreground face nil t)))
      (and fg (not (member fg '("unspecified-fg" "unspecified"))) fg)))
   (t nil)))

(defun elfeed-nano--icon-accessor (name)
  "Return the appropriate nerd-icons insertion function for NAME."
  (cond ((string-prefix-p "nf-fa-" name)        #'nerd-icons-faicon)
        ((string-prefix-p "nf-oct-" name)       #'nerd-icons-octicon)
        ((string-prefix-p "nf-cod-" name)       #'nerd-icons-codicon)
        ((string-prefix-p "nf-dev-" name)       #'nerd-icons-devicon)
        ((string-prefix-p "nf-seti-" name)      #'nerd-icons-sucicon)
        ((string-prefix-p "nf-weather-" name)   #'nerd-icons-wicon)
        ((string-prefix-p "nf-pom-" name)       #'nerd-icons-pomicon)
        ((string-prefix-p "nf-powerline-" name) #'nerd-icons-powerline)
        ((string-prefix-p "nf-fl-" name)        #'nerd-icons-flicon)
        ((string-prefix-p "nf-ips-" name)       #'nerd-icons-ipsicon)
        (t #'nerd-icons-mdicon)))

(defun elfeed-nano--icon (feed unread &optional background)
  "Return a colored icon string for FEED, bright when UNREAD.
BACKGROUND is an optional stripe background color to merge in."
  (let* ((info (elfeed-nano--feed-icon-info feed))
         (name (car info))
         (icon-face (cdr info))
         (accessor (elfeed-nano--icon-accessor name))
         (glyph (funcall accessor name))
         (face-props (plist-get (text-properties-at 0 glyph) 'face))
         (face-props (if (listp face-props) face-props (list face-props)))
         (fg (cond ((and (not unread) (not elfeed-nano-color-read-icons))
                    (elfeed-nano--face-fg 'font-lock-comment-face))
                   (icon-face
                    (or (elfeed-nano--face-fg icon-face)
                        (elfeed-nano--face-fg 'nerd-icons-orange)
                        (elfeed-nano--face-fg 'default)))
                   ((equal name "nf-md-rss")
                    (or (elfeed-nano--face-fg 'nerd-icons-orange)
                        (elfeed-nano--face-fg 'warning)))
                   (t
                    (elfeed-nano--face-fg 'default))))
         (spec (append (list :foreground (or fg "gray")) face-props)))
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
  "Return a short relative date string for TIME (epoch seconds).
Return \"unknown\" when TIME is nil or not a number."
  (if (not (numberp time))
      "unknown"
    (let* ((diff (- (float-time) time))
           (secs (abs diff)))
      (cond ((< secs 60) "just now")
            ((< secs 3600) (format "%dm ago" (max 1 (floor (/ secs 60)))))
            ((< secs 86400) (format "%dh ago" (max 1 (floor (/ secs 3600)))))
            ((< secs 172800) "yesterday")
            ((< secs (* 7 86400)) (format "%dd ago" (floor (/ secs 86400))))
            ((< secs (* 30 86400)) (format "%dw ago" (floor (/ secs (* 7 86400)))))
            (t (format-time-string "%Y-%m-%d" time))))))

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
  (when (bound-and-true-p elfeed-show-entry-delete)
    (funcall elfeed-show-entry-delete))
  (with-current-buffer (elfeed-search-buffer)
    (when (elfeed-search--remain-on-entry-p 'show) (elfeed-nano-next-entry))
    (call-interactively #'elfeed-search-show-entry)))

(defun elfeed-nano-show-prev ()
  "Show the previous entry in the search buffer."
  (interactive)
  (when (bound-and-true-p elfeed-show-entry-delete)
    (funcall elfeed-show-entry-delete))
  (with-current-buffer (elfeed-search-buffer)
    (when (elfeed-search--remain-on-entry-p 'show) (elfeed-nano-prev-entry))
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

(defvar elfeed-nano--saved-state nil
  "Saved state for toggling elfeed-nano-mode off.")

(defvar-keymap elfeed-nano-mode-map
  "n"       #'elfeed-nano-next-entry
  "p"       #'elfeed-nano-prev-entry
  "<down>"  #'elfeed-nano-next-entry
  "<up>"    #'elfeed-nano-prev-entry)

(defun elfeed-nano--on ()
  "Install the nano-style elfeed UI."
  (setq elfeed-nano--saved-state
        (list elfeed-search-print-entry-function))
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

(defun elfeed-nano--off ()
  "Remove the nano-style elfeed UI."
  (pcase-let ((`(,print-entry) elfeed-nano--saved-state))
    (setq elfeed-search-print-entry-function print-entry))
  (remove-hook 'elfeed-search-mode-hook #'elfeed-nano-search-mode)
  (remove-hook 'elfeed-show-mode-hook #'elfeed-nano-show-mode)
  (add-hook 'elfeed-search-update-hook #'elfeed-search-add-separators)
  (advice-remove 'elfeed-search--update-line #'elfeed-nano--update-line)
  (advice-remove 'elfeed-search--remove-marked-overlay #'elfeed-nano--remove-marked-overlay)
  (advice-remove 'elfeed-search--make-marked-overlay #'elfeed-nano--make-marked-overlay)
  (with-eval-after-load 'elfeed-search
    (keymap-set elfeed-search-mode-map "n" #'elfeed-search-show-entry)
    (keymap-set elfeed-search-mode-map "p" #'elfeed-search-show-entry)
    (keymap-set elfeed-search-mode-map "<down>" #'elfeed-search-next-entry)
    (keymap-set elfeed-search-mode-map "<up>" #'elfeed-search-previous-entry))
  (with-eval-after-load 'elfeed-show
    (keymap-set elfeed-show-mode-map "n" #'elfeed-show-next)
    (keymap-set elfeed-show-mode-map "p" #'elfeed-show-previous))
  (setq elfeed-nano--saved-state nil))

;;;###autoload
(define-minor-mode elfeed-nano-mode
  "Lightweight nano-style elfeed UI."
  :lighter " eN"
  :keymap elfeed-nano-mode-map
  (if elfeed-nano-mode
      (elfeed-nano--on)
    (elfeed-nano--off)))

(provide 'elfeed-nano)
;;; elfeed-nano.el ends here
