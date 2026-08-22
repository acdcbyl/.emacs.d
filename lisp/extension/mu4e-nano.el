;;; mu4e-nano.el --- Lightweight nano-style mu4e headers view -*- lexical-binding: t -*-

;;; Commentary:
;;
;; A minimal re-implementation of the nano-mu4e headers view.  One good
;; look: two-line message cards with nerd-icons status glyphs, relative
;; dates, thread indentation and zebra stripes.  Depends only on `mu4e'
;; and `nerd-icons' (both already installed).
;;
;;   Line 1:  [status] [domain] Sender .............. relative date
;;   Line 2:           subject  [📎][🔒][🛡] [tags]
;;
;; mu4e's own keybindings, marks, view and actions stay untouched; only
;; n/p/↑/↓ are re-bound to message-aware navigation (entries are two
;; lines tall).
;;
;; Usage (add to init-email.el):
;;
;;   (use-package mu4e-nano
;;     :ensure nil
;;     :after mu4e
;;     :hook (mu4e-headers-mode . mu4e-nano-mode))

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'mu4e)
(require 'nerd-icons)

(defgroup mu4e-nano nil
  "A lightweight nano-style mu4e headers view."
  :group 'mu4e)

;; ---------------------------------------------------------------------------
;; Faces (stripe/highlight colors adapt to the active theme)
;; ---------------------------------------------------------------------------

(defface mu4e-nano-unread-face
  '((t :inherit bold :extend t))
  "Face for unread messages (sender, subject and status icon).")

(defface mu4e-nano-read-face
  '((t :inherit default :extend t))
  "Face for read messages (regular weight; unread uses the bold face).")

(defface mu4e-nano-new-face
  '((t :inherit (bold warning) :extend t))
  "Face for brand-new messages.")

(defface mu4e-nano-flagged-face
  '((t :inherit (bold error) :extend t))
  "Face for flagged messages.")

(defface mu4e-nano-draft-face
  '((t :inherit (font-lock-comment-face warning) :extend t))
  "Face for drafts.")

(defface mu4e-nano-sent-face
  '((t :inherit font-lock-comment-face :extend t))
  "Face for sent/forwarded messages.")

(defface mu4e-nano-replied-face
  '((t :inherit (bold success) :extend t))
  "Face for the replied status icon.")

(defface mu4e-nano-passed-face
  '((t :inherit (bold link) :extend t))
  "Face for the forwarded (passed) status icon.")

(defface mu4e-nano-trash-face
  '((t :inherit font-lock-comment-face :extend t))
  "Face for trashed messages.")

(defface mu4e-nano-junk-face
  '((t :inherit (bold error) :extend t))
  "Face for junk/spam messages.")

(defface mu4e-nano-date-face
  '((t :inherit font-lock-comment-face :extend t))
  "Face for the relative date.")

(defface mu4e-nano-tags-face
  '((t :inherit font-lock-comment-face :extend t))
  "Face for message tags.")

(defface mu4e-nano-stripe-face
  '((t :extend t))
  "Background of alternating rows.  Colors set from the active theme.")

(defface mu4e-nano-hl-face
  '((t :extend t))
  "Background of the current entry.  Colors set from the active theme.")

(defun mu4e-nano--dark-p (color)
  "Return non-nil when COLOR is a dark color."
  (when-let* ((rgb (color-name-to-rgb color)))
    (< (+ (* 0.299 (nth 0 rgb))
          (* 0.587 (nth 1 rgb))
          (* 0.114 (nth 2 rgb)))
       0.5)))

(defun mu4e-nano--update-theme-faces ()
  "Adapt stripe/highlight backgrounds to the theme.
Status-icon colors follow the active theme automatically via `:inherit'
on their faces (see `mu4e-nano-status-icons'); no hardcoded colors here."
  (let* ((bg (face-background 'default))
         (dark (or (and bg (color-defined-p bg) (mu4e-nano--dark-p bg)) t)))
    (when (and bg (color-defined-p bg))
      (set-face-attribute 'mu4e-nano-stripe-face nil :background
                          (if dark (color-lighten-name bg 2)
                            (color-darken-name bg 2)))
      (set-face-attribute 'mu4e-nano-hl-face nil :background
                          (if dark (color-lighten-name bg 7)
                            (color-darken-name bg 5))))))

(when (boundp 'after-load-theme-hook)
  (add-hook 'after-load-theme-hook #'mu4e-nano--update-theme-faces))

;; ---------------------------------------------------------------------------
;; Small helpers
;; ---------------------------------------------------------------------------

(defun mu4e-nano--face-fg (face)
  "Return the foreground color of FACE, or nil when unspecified.
Resolves inherited foregrounds so face `:inherit' chains (theme
semantic faces such as `error' / `warning') are honored."
  (let ((fg (face-foreground face nil t)))
    (and fg (not (equal fg "unspecified-fg")) fg)))

(defun mu4e-nano--glyph (name face &optional background)
  "Return a propertized nerd-icons glyph NAME, colored with FACE.
BACKGROUND is an optional stripe color merged into the face.  Both
`face' and `font-lock-face' are set so the Nerd Font family is kept.
The nerd-icons accessor is chosen from the name prefix (nf-md-, nf-fa-...)."
  (let* ((accessor (cond ((string-prefix-p "nf-fa-" name)
                          #'nerd-icons-faicon)
                         ((string-prefix-p "nf-oct-" name)
                          #'nerd-icons-octicon)
                         ((string-prefix-p "nf-cod-" name)
                          #'nerd-icons-codicon)
                         (t #'nerd-icons-mdicon)))
         (glyph (funcall accessor name))
         (gface (plist-get (text-properties-at 0 glyph) 'face))
         (gface (if (listp gface) gface (list gface)))
         (fg (mu4e-nano--face-fg face))
         (spec (append (and fg (list :foreground fg)) gface)))
    (when background
      (setq spec (plist-put spec :background background)))
    (propertize glyph 'face spec 'font-lock-face spec)))

(defun mu4e-nano--truncate (str width)
  "Return STR truncated to WIDTH columns, with an ellipsis when cut."
  (if (> (string-width str) width)
      (concat (truncate-string-to-width str (max 1 (1- width))) "…")
    str))

(defun mu4e-nano--relative-date (time)
  "Return a short relative date string for TIME.
TIME can be epoch seconds or an Emacs time value (list).
Return \"unknown\" when TIME is nil."
  (if (null time)
      "unknown"
    (let* ((secs (abs (- (float-time) (float-time time)))))
      (cond ((< secs 60) "just now")
            ((< secs 3600) (format "%dm ago" (max 1 (floor (/ secs 60)))))
            ((< secs 86400) (format "%dh ago" (max 1 (floor (/ secs 3600)))))
            ((< secs 172800) "yesterday")
            ((< secs (* 7 86400)) (format "%dd ago" (floor (/ secs 86400))))
            ((< secs (* 30 86400)) (format "%dw ago" (floor (/ secs (* 7 86400)))))
            (t (format-time-string "%Y-%m-%d" time))))))

(defun mu4e-nano--contact-name (contact)
  "Extract the display name from a mu4e CONTACT, in any known format.
Handles (name . email) conses, (:name .. :email ..) plists, the
(\"email\" :name ..) shape and plain email strings."
  (cond ((consp contact)
         (cond ((stringp (car contact))
                (if (stringp (cdr contact))
                    (car contact)                 ; (name . email)
                  (plist-get (cdr contact) :name))); ("email" :name ..)
               (t (plist-get contact :name))))     ; (:name .. :email ..)
        (t nil)))

(defun mu4e-nano--contact-email (contact)
  "Extract the email from a mu4e CONTACT, in any known format."
  (cond ((consp contact)
         (cond ((stringp (car contact))
                (if (stringp (cdr contact))
                    (cdr contact)                 ; (name . email)
                  (car contact)))                 ; ("email" :name ..)
               (t (or (plist-get contact :email)
                      (plist-get contact :mail))))); (:name .. :email ..)
        ((stringp contact) contact)
        (t nil)))

(defun mu4e-nano--sender (msg)
  "Return the display name (or email) of the sender of MSG."
  (let* ((contact (car (mu4e-message-field msg :from)))
         (name (mu4e-nano--contact-name contact))
         (email (mu4e-nano--contact-email contact)))
    (or (and name (not (equal name "")) name) email "(unknown)")))

(defun mu4e-nano--sender-domain (msg)
  "Return the domain of the sender's email address, or nil."
  (let ((email (mu4e-nano--contact-email
                (car (mu4e-message-field msg :from)))))
    (when-let* ((email (and (stringp email) email))
                ((string-match "@\\([^@]+\\)$" email)))
      (match-string 1 email))))

;; ---------------------------------------------------------------------------
;; Sender icons (by email domain)
;; ---------------------------------------------------------------------------

(defcustom mu4e-nano-sender-icons
  '(("github\\.com"        . "nf-fa-github")
    ("twitter\\.com\\|x\\.com" . "nf-fa-twitter")
    ("youtube\\.com"       . "nf-fa-youtube")
    ("reddit\\.com"        . "nf-fa-reddit")
    ("telegram"            . "nf-fa-telegram")
    ("weibo"               . "nf-fa-weibo")
    ("weixin\\.qq\\.com\\|qq\\.com" . "nf-fa-weixin")
    ("zhihu"               . "nf-fa-comments")
    ("linkedin"            . "nf-fa-linkedin")
    ("stackoverflow\\|stackexchange" . "nf-fa-terminal"))
  "Alist of (domain-regexp . nerd-icons name) for sender email domains.
The sender's email domain is matched against each regexp; the first
match wins.  Other senders get no domain icon."
  :type '(alist :key-type (string :tag "Domain regexp")
                :value-type (string :tag "nerd-icons name"))
  :group 'mu4e-nano)

(defun mu4e-nano--domain-icon (msg)
  "Return a domain icon for the sender of MSG, or nil."
  (let ((domain (mu4e-nano--sender-domain msg)))
    (when domain
      (let ((name (cdr (cl-find-if (lambda (pair)
                                     (string-match-p (car pair) domain))
                                   mu4e-nano-sender-icons))))
        (when name
          (mu4e-nano--glyph name 'shadow))))))

;; ---------------------------------------------------------------------------
;; Status glyphs & subject-line indicators
;; ---------------------------------------------------------------------------

(defcustom mu4e-nano-status-icons
  '((new      . ("nf-md-email"   . mu4e-nano-new-face))
    (unread   . ("nf-md-email"   . mu4e-nano-unread-face))
    (flagged  . ("nf-md-flag"    . mu4e-nano-flagged-face))
    (draft    . ("nf-md-pencil"  . mu4e-nano-draft-face))
    (replied  . ("nf-md-reply"   . mu4e-nano-replied-face))
    (passed   . ("nf-md-send"    . mu4e-nano-passed-face))
    (trashed  . ("nf-fa-trash"   . mu4e-nano-trash-face))
    (junk     . ("nf-md-alert"   . mu4e-nano-junk-face))
    (read     . ("nf-fa-envelope" . mu4e-nano-read-face)))
  "Alist of (flag . (icon-name . face)) for the status column.
FLAG is one of new, unread, flagged, draft, replied, passed, trashed,
junk or read (the fallback for plain read messages).  The first flag
in this list carried by a message wins.  ICON-NAME is a nerd-icons
name (nf-md- / nf-fa- / nf-oct- prefix selects the icon set); the FACE
controls the icon color (customize it, or the theme-derived defaults
in `mu4e-nano--update-theme-faces')."
  :type '(alist :key-type symbol
                :value-type (cons (string :tag "Icon name")
                                  (face :tag "Face")))
  :group 'mu4e-nano)

(defun mu4e-nano--status-glyph (msg)
  "Return (icon-name . face) for the status glyph of MSG."
  (let ((flags (mu4e-message-field msg :flags)))
    (or (cdr (cl-find-if (lambda (entry)
                           (memq (car entry) flags))
                         mu4e-nano-status-icons))
        (cdr (assq 'read mu4e-nano-status-icons)))))

(defun mu4e-nano--indicators (msg)
  "Return indicator glyphs (attachment/signature/encryption) for MSG."
  (let ((flags (mu4e-message-field msg :flags))
        (attach (mu4e-message-field msg :attachments))
        (parts nil))
    (when (> (or attach 0) 0)
      (push (mu4e-nano--glyph "nf-md-paperclip" 'shadow) parts))
    (when (memq 'signed flags)
      (push (mu4e-nano--glyph "nf-md-shield" 'shadow) parts))
    (when (memq 'encrypted flags)
      (push (mu4e-nano--glyph "nf-md-lock" 'shadow) parts))
    (if parts
        (concat " " (apply #'concat (nreverse parts)))
      "")))

(defun mu4e-nano--tags (msg)
  "Return a propertized tag string for MSG, or \"\"."
  (let ((tags (mu4e-message-field msg :tags)))
    (if tags
        (propertize (concat " " (mapconcat #'symbol-name tags " "))
                    'face 'mu4e-nano-tags-face)
      "")))

;; ---------------------------------------------------------------------------
;; Rendering
;; ---------------------------------------------------------------------------

(defvar-local mu4e-nano--message-list nil
  "Messages collected while a search is in progress.")

(defun mu4e-nano--insert-message (msg)
  "Insert the two-line card for MSG at point."
  (let* ((docid (mu4e-message-field msg :docid))
         (flags (mu4e-message-field msg :flags))
         (unread (or (memq 'unread flags) (memq 'new flags)))
         (meta (mu4e-message-field msg :meta))
         (level (if mu4e-search-threads (or (plist-get meta :level) 0) 0))
         (indent (min 8 (* 2 level)))
         (sender (mu4e-nano--sender msg))
         (subject (or (mu4e-message-field msg :subject) "(no subject)"))
         (subject (cond ((memq 'junk flags) (concat "[SPAM] " subject))
                        ((memq 'trashed flags) (concat "[TRASH] " subject))
                        (t subject)))
         (date (mu4e-nano--relative-date (mu4e-message-field msg :date)))
         (status (mu4e-nano--status-glyph msg))
         ;; Stateless zebra: each entry is two lines, so the entry index is
         ;; (count-lines)/2; odd entries get the stripe.  Works both for the
         ;; initial full render and for single-entry updates.
         (stripe (when (= 1 (mod (/ (count-lines (point-min) (point)) 2) 2))
                   'mu4e-nano-stripe-face))
         (stripe-bg (and stripe
                         (face-background 'mu4e-nano-stripe-face)))
         (status-icon (mu4e-nano--glyph (car status) (cdr status) stripe-bg))
         (domain-icon (mu4e-nano--domain-icon msg))
         (status-w (+ 2 (if domain-icon 2 0)))
         (fringe-w (length mu4e--mark-fringe))
         (width (max 30 (- (window-width) fringe-w status-w indent)))
         (date-w (+ (string-width date) 2))
         (face (if unread 'mu4e-nano-unread-face 'mu4e-nano-read-face))
         (stripe-face (when (= 1 (mod (/ (count-lines (point-min) (point)) 2) 2))
                        'mu4e-nano-stripe-face))
         (line-face (delq nil (list face stripe-face)))
         (date-face (delq nil (list (if unread 'default 'mu4e-nano-date-face)
                                    stripe-face)))
         (sub-face (delq nil (list face stripe-face)))
         (sender (mu4e-nano--truncate sender (max 10 (- width date-w))))
         (subject (mu4e-nano--truncate subject width)))
    (let ((beg (point)))
      (insert (mu4e~headers-docid-cookie docid)
              (propertize mu4e--mark-fringe 'face line-face)
              status-icon                      ; own face: Nerd Font family
              (or domain-icon "")
              (propertize " " 'face line-face)
              (propertize (make-string indent ?\s) 'face line-face)
              (propertize sender 'face line-face)
              (propertize " " 'face line-face
                          'display `(space :align-to (- right ,date-w)))
              (propertize date 'face date-face)
              "\n"
              (propertize (make-string (+ status-w indent) ?\s)
                          'face sub-face)
              (propertize subject 'face sub-face)
              (mu4e-nano--indicators msg)
              (mu4e-nano--tags msg)
              "\n")
      ;; mark the whole entry with the message and docid, like mu4e does
      (add-text-properties beg (point) (list 'msg msg 'docid docid)))))

(defun mu4e-nano--hl-line-range ()
  "Range covering the whole two-line entry for hl-line."
  (save-excursion
    (beginning-of-line)
    (unless (get-text-property (point) 'docid)
      (forward-line -1))
    (let ((beg (line-beginning-position)))
      (forward-line 2)
      (cons beg (max beg (min (point-max) (line-beginning-position)))))))

;; ---------------------------------------------------------------------------
;; Handlers (replace the default mu4e header rendering)
;; ---------------------------------------------------------------------------

(defun mu4e-nano-append-handler (msglst)
  "Collect MSGLST for rendering once the search has finished."
  (when (buffer-live-p (mu4e-get-headers-buffer))
    (with-current-buffer (mu4e-get-headers-buffer)
      (setq-local mu4e-nano--message-list
                  (append mu4e-nano--message-list msglst)))))

(defun mu4e-nano-found-handler (&optional count)
  "Render all collected messages, then run mu4e's default post-processing."
  (when (buffer-live-p (mu4e-get-headers-buffer))
    (with-current-buffer (mu4e-get-headers-buffer)
      (setq-local hl-line-range-function #'mu4e-nano--hl-line-range
                  hl-line-face 'mu4e-nano-hl-face)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (seq-do #'mu4e-nano--insert-message mu4e-nano--message-list))))
  ;; end-of-results marker, target jumps, highlight, thread mode, header line
  (mu4e~headers-found-handler count))

(defun mu4e-nano-erase-handler (&optional text)
  "Clear the headers buffer and reset the collection state."
  (mu4e~headers-clear text)
  (when (buffer-live-p (mu4e-get-headers-buffer))
    (with-current-buffer (mu4e-get-headers-buffer)
      (setq-local mu4e-nano--message-list nil))))

(defun mu4e-nano--remove-entry (docid)
  "Delete the two-line entry with DOCID from the headers buffer."
  (when-let* ((buf (mu4e-get-headers-buffer))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (when (mu4e~headers-goto-docid docid)
        (let ((inhibit-read-only t)
              (beg (line-beginning-position)))
          (delete-region beg (min (point-max) (line-beginning-position 3))))))))

(defun mu4e-nano-remove-handler (docid)
  "Remove the two-line entry with DOCID and close its view if needed."
  (when (buffer-live-p (mu4e-get-headers-buffer))
    (mu4e-nano--remove-entry docid))
  (when (and (mu4e~headers-view-this-message-p docid)
             (buffer-live-p (mu4e-get-view-buffer)))
    (let ((buf (mu4e-get-view-buffer)))
      (mapc #'delete-window (get-buffer-window-list buf nil t))
      (kill-buffer buf))))

(defun mu4e-nano-update-handler (msg is-move maybe-view)
  "In-place update of the entry for MSG (flags changed, etc.)."
  (when (buffer-live-p (mu4e-get-headers-buffer))
    (with-current-buffer (mu4e-get-headers-buffer)
      (let* ((docid (mu4e-message-field msg :docid))
             (point (mu4e~headers-docid-pos docid))
             (initial (mu4e~headers-docid-at-point))
             (inhibit-read-only t))
        (when point
          (when (mu4e-mark-docid-marked-p docid)
            (mu4e-mark-set 'unmark))
          ;; re-use the thread info from the old entry
          (plist-put msg :meta (mu4e~headers-field-for-docid docid :meta))
          (mu4e-nano--remove-entry docid)
          (when (and maybe-view (mu4e~headers-view-this-message-p docid))
            (save-excursion (mu4e-view msg)))
          (unless is-move
            (save-excursion
              (goto-char point)
              (mu4e-nano--insert-message msg)))
          (if (and initial (mu4e~headers-goto-docid initial))
              (mu4e~headers-highlight initial)
            (mu4e~headers-highlight docid))
          (run-hooks 'mu4e-message-changed-hook))))))

;; ---------------------------------------------------------------------------
;; Navigation
;; ---------------------------------------------------------------------------

(defun mu4e-nano-next-msg (&optional n)
  "Move to the Nth next message."
  (interactive "p")
  (mu4e--in-headers-context
   (let ((moved nil))
     (dotimes (_ (or n 1))
       (when (mu4e-headers-find-if-next (lambda (_) t))
         (setq moved t)))
     (if moved
         (mu4e~headers-highlight (mu4e~headers-docid-at-point))
       (mu4e-message "No next message")))))

(defun mu4e-nano-prev-msg (&optional n)
  "Move to the Nth previous message."
  (interactive "p")
  (mu4e--in-headers-context
   (let ((moved nil))
     (dotimes (_ (or n 1))
       (when (mu4e-headers-find-if-next (lambda (_) t) t)
         (setq moved t)))
     (if moved
         (mu4e~headers-highlight (mu4e~headers-docid-at-point))
       (mu4e-message "No previous message")))))

;; ---------------------------------------------------------------------------
;; Minor mode
;; ---------------------------------------------------------------------------

(defvar mu4e-nano--saved-handlers nil
  "Saved mu4e handler variables, for toggling the mode off.")

(defvar mu4e-nano--saved-hl-line nil
  "Saved hl-line variables, for toggling the mode off.")

(defun mu4e-nano--on ()
  "Install the nano header rendering handlers."
  (setq mu4e-nano--saved-handlers
        (list mu4e-headers-append-func mu4e-found-func mu4e-erase-func
              mu4e-update-func mu4e-remove-func mu4e-headers-fields))
  (setq mu4e-headers-append-func #'mu4e-nano-append-handler
        mu4e-found-func #'mu4e-nano-found-handler
        mu4e-erase-func #'mu4e-nano-erase-handler
        mu4e-update-func #'mu4e-nano-update-handler
        mu4e-remove-func #'mu4e-nano-remove-handler
        mu4e-headers-fields nil)
  ;; Save hl-line variables from the headers buffer if available.
  (when-let* ((buf (mu4e-get-headers-buffer)))
    (with-current-buffer buf
      (setq mu4e-nano--saved-hl-line
            (list (buffer-local-value 'hl-line-range-function buf)
                  (buffer-local-value 'hl-line-face buf)))))
  ;; `mu4e-view' calls `mu4e~headers-update-handler' directly (not via
  ;; `mu4e-update-func'); that default handler only knows the one-line
  ;; layout and would clobber the first line of our two-line entries.
  (advice-add 'mu4e~headers-update-handler :override #'mu4e-nano-update-handler)
  (mu4e-nano--update-theme-faces))

(defun mu4e-nano--off ()
  "Restore the default mu4e header rendering."
  (advice-remove 'mu4e~headers-update-handler #'mu4e-nano-update-handler)
  (pcase-let ((`(,append ,found ,erase ,update ,remove ,fields)
               mu4e-nano--saved-handlers))
    (setq mu4e-headers-append-func append
          mu4e-found-func found
          mu4e-erase-func erase
          mu4e-update-func update
          mu4e-remove-func remove
          mu4e-headers-fields fields))
  ;; Restore hl-line variables in the headers buffer if available.
  (when-let* ((buf (mu4e-get-headers-buffer))
              (saved mu4e-nano--saved-hl-line))
    (with-current-buffer buf
      (setq-local hl-line-range-function (nth 0 saved)
                  hl-line-face (nth 1 saved))))
  (setq mu4e-nano--saved-handlers nil
        mu4e-nano--saved-hl-line nil))

(defvar-keymap mu4e-nano-mode-map
  "n"     #'mu4e-nano-next-msg
  "p"     #'mu4e-nano-prev-msg
  "<down>" #'mu4e-nano-next-msg
  "<up>"   #'mu4e-nano-prev-msg)

;;;###autoload
(define-minor-mode mu4e-nano-mode
  "Lightweight nano-style mu4e headers view.

Each message is rendered as a two-line card with a nerd-icons status
glyph, sender, relative date, subject and thread indentation.

Add `mu4e-headers-mode' to `mu4e-nano-mode-hook' (or use
`:hook (mu4e-headers-mode . mu4e-nano-mode)' in use-package) to
enable it in every headers buffer."
  :lighter " µN"
  :keymap mu4e-nano-mode-map
  (if mu4e-nano-mode
      (mu4e-nano--on)
    (mu4e-nano--off)))

(provide 'mu4e-nano)
;;; mu4e-nano.el ends here
