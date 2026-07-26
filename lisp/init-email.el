;;; init-email.el --- The email settings -*- lexical-binding: t -*-

;;; Commentary:
;;  Configured for mu4e with Gmail and mbsync.

;;; Code:
(use-package auth-source-pass
  :ensure nil
  :config
  (auth-source-pass-enable))

(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :defer t
  :commands (mu4e mu4e-transient-menu)
  :init
  (setq mail-user-agent 'mu4e)
  :config
  ;;;; Basics
  (setq user-mail-address "wtchel088@gmail.com"
        user-full-name "Aiser Han")

  ;;;; Retrieving mail
  ;; Command to sync mail (mbsync is used here)
  (setq mu4e-get-mail-command "mbsync -a"
        mu4e-index-lazy-check t
        mu4e-update-interval 180)

  ;;;; Folders
  ;; Note: Gmail folders are case-sensitive and often prefixed with [Gmail]/
  (setq mu4e-inbox-folder  "/INBOX"
        mu4e-sent-folder   "/[Gmail]/已发邮件"
        mu4e-drafts-folder "/[Gmail]/草稿"
        mu4e-trash-folder  "/[Gmail]/垃圾邮件")

  ;; Fix duplicate UID errors when using mbsync and mu4e
  (setq mu4e-change-filenames-when-moving t)

  ;; Gmail specific: don't save sent messages to Sent, Gmail does it automatically
  (setq mu4e-sent-messages-behavior 'delete)

  ;;;; Shortcuts
  ;; Align these with actual folder names for quick navigation
  (setq mu4e-maildir-shortcuts
        '((:maildir "/INBOX"              :key ?i)
          (:maildir "/[Gmail]/已发邮件"  :key ?s)
          (:maildir "/[Gmail]/草稿"     :key ?d)
          (:maildir "/[Gmail]/垃圾邮件"        :key ?t)
          ;; (:maildir "/lists"              :key ?l)
          )
        )

  ;;;; Visuals
  (setq mu4e-use-fancy-chars t
        mu4e-headers-draft-mark    '("D" . "🧪")
        mu4e-headers-flagged-mark  '("F" . "🚩")
        mu4e-headers-new-mark      '("N" . "✨")
        mu4e-headers-unread-mark   '("u" . "✉️"))

  ;; Use nerd-icons if available
  (with-eval-after-load "nerd-icons"
    (setq mu4e-file-name-to-icon-function #'nerd-icons-icon-for-file))

;;;; Sending mail
  (setq send-mail-function #'smtpmail-send-it
        message-send-mail-function #'smtpmail-send-it

        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'ssl

        ;; SMTP login user
        smtpmail-smtp-user "wtchel088@gmail.com"

        ;; Debug
        ;; smtpmail-debug-info t
        ;; smtpmail-debug-verb t

        ;; Don't ask repeatedly
        auth-source-cache-expiry nil)

  ;;;; Extras
  (setq mu4e-attachment-dir "~/Downloads")

  ;; Show messages in browser with 'a V'
  (add-to-list 'mu4e-view-actions '("ViewBrowser" . mu4e-action-view-in-browser) t)

  ;; Preferred format
  (with-eval-after-load "mm-decode"
    (setq mm-discouraged-alternatives
          (append '("text/html" "text/richtext")
                  mm-discouraged-alternatives)))

  ;; Composer tweaks
  (defun aiser/mu4e-compose-setup ()
    "Custom settings for mu4e compose mode."
    (set-fill-column 72)
    (flyspell-mode 1))
  (add-hook 'mu4e-compose-mode-hook #'aiser/mu4e-compose-setup)

  ;; Enable gnus-dired-mode for attaching files from dired
  (add-hook 'dired-mode-hook #'gnus-dired-mode)
  :general
  (aiser/leader-def
    "oe"  'mu4e-transient-menu))
;; (use-package nano-mu4e
;;   :load-path "nano-mu4e"
;;   :after mu4e
;;   :commands (nano-mu4e-mode nano-mu4e)
;;   :hook (mu4e-headers-mode . nano-mu4e-mode)
;;   :config
;;   (setq nano-mu4e-view-style 'simple)
;;   ;; (setq nano-mu4e-tag-style 'round)
;;   (setq nano-mu4e-msg-preview t))
(provide 'init-email)

;;; init-email.el ends here
