;;; init-telega.el --- Telegram for emacs -*- lexical-binding: t no-byte-compile: t-*-
;;; Commentary:
;;; Code:

(use-package telega
  :ensure t
  :commands telega

  :init
  ;; Basic directory paths
  (setq telega-directory "~/.local/share/telega")
  (setq telega-msg-save-dir "~/Downloads")
  (setq telega-server-libs-prefix "/usr")

  ;; ========================================================================
  ;; Avatar and Image Settings (Fix for missing profile pictures)
  ;; ========================================================================
  ;; Enable image support
  (setq telega-use-images t)

  ;; Disable SVG base-uri mode and force Base64 inline image embedding.
  ;; Solves librsvg security/permission issues on Linux preventing local photo loading.
  (setq telega-use-svg-base-uri nil)

  ;; Enable avatar display across all interfaces
  (setq telega-root-show-avatars t)                ; Root view chat list avatars
  (setq telega-user-show-avatars t)                ; User avatars
  (setq telega-chat-show-avatars t)                ; Chat buffer avatars
  (setq telega-active-locations-show-avatars t)    ; Sidebars / active locations avatars
  (setq telega-completions-username-show-avatars t); User completion popup avatars

  ;; Workaround for font-height sliced avatar gaps in Emacs GUI
  (setq telega-avatar-workaround-gaps-for '(all))

  ;; ========================================================================
  ;; UI and Layout Preferences
  ;; ========================================================================
  ;; Default target language for translation
  (setq telega-translate-to-language-by-default "zh")

  ;; Message fill column
  (setq telega-chat-fill-column 90)

  ;; Sticker size (height . width) in char units
  (setq telega-sticker-size '(6 . 24))

  ;; Clean button display without brackets [...]
  (setq telega-brackets nil)

  ;; Group consecutive message timestamps from same sender
  (setq telega-squash-message-timestamps t)

  ;; Supported chat input markups
  (setq telega-chat-input-markups '("markdown2" "org"))

  ;; Root view configuration
  (setq telega-root-default-view-function #'telega-view-folders)
  (setq telega-root-keep-cursor 'track)
  (setq telega-root-buffer-name "*Telega Root*")

  ;; Hide chat folder icon prefixes for clean layout
  (setq telega-chat-folders-insexp (lambda () nil))

  ;; Filter tabs at the top of root view
  (setq telega-filters-custom
        '(("All" . main)
          ("Unread" . unread)
          ("Groups" . (type basicgroup supergroup))
          ("Channels" . (type channel))
          ("Bots" . (type bot))))
  (setq telega-filter-custom-show-folders t)

  ;; Use native emoji fonts instead of PNG image files for maximum performance
  (setq telega-emoji-use-images nil)

  :config
  ;; Enable desktop notifications
  (telega-notifications-mode 1)

  ;; Auto-play GIFs and animated stickers
  (setq telega-autoplay-mode t)

  ;; File opening function
  (setq telega-open-file-function #'org-open-file)

  ;; Auto-fit images to window width
  (add-hook 'telega-image-mode-hook #'image-transform-fit-to-window)

  ;; Enable Markdown enhancement mode if available
  (when (fboundp 'telega-mnz-mode)
    (global-telega-mnz-mode 1))

  ;; Chat buffer setup hook
  (add-hook 'telega-chat-mode-hook
            (lambda ()
              ;; Enable completion-at-point-functions
              (telega-completions-setup-capf)
              ;; Disable electric-pair to prevent auto-pairing interference
              (electric-pair-local-mode -1)))

  ;; ========================================================================
  ;; Symbol and Icon Customizations (using nerd-icons)
  ;; ========================================================================
  (when (require 'nerd-icons nil t)
    (setq telega-symbol-button-close (nerd-icons-mdicon "nf-md-close_box_outline")
          telega-symbol-verified (nerd-icons-codicon "nf-cod-verified_filled" :face 'telega-blue)
          telega-symbol-vertical-bar "│" ;; U+2502 Box Drawings Light Vertical
          telega-symbol-saved-messages-tag-end (nerd-icons-faicon "nf-fa-tag")
          telega-symbol-forum (nerd-icons-mdicon "nf-md-format_list_text")
          telega-symbol-flames (nerd-icons-mdicon "nf-md-delete_clock")
          telega-symbol-mark (propertize " " 'face 'telega-button-highlight)
          telega-symbol-reply (nerd-icons-faicon "nf-fa-reply" :face 'ansi-color-blue)
          telega-symbol-reply-quote (nerd-icons-faicon "nf-fa-reply_all" :face 'ansi-color-blue)
          telega-symbol-forward (nerd-icons-faicon "nf-fa-mail_forward")
          telega-symbol-checkmark (nerd-icons-mdicon "nf-md-check")
          telega-symbol-heavy-checkmark (nerd-icons-codicon "nf-cod-check_all")
          telega-symbol-summarize-in (nerd-icons-octicon "nf-oct-fold")
          telega-symbol-summarize-out (nerd-icons-octicon "nf-oct-unfold")))

  ;; ;; ========================================================================
  ;; ;; Keybindings
  ;; ;; ========================================================================
  ;; (define-key telega-prefix-map (kbd "p") #'telega-chatbuf-filter-search)
  ;; (define-key telega-prefix-map (kbd "d") #'telega-chat-remove-member)
  ;; (define-key telega-prefix-map (kbd "m") #'telega-describe-chat-members)
  ;; (define-key telega-prefix-map (kbd "h") #'telega-notifications-history)
  ;; (define-key telega-prefix-map (kbd "x") #'telega-chatbuf-thread-cancel)

  ;; ;; Global prefix keybinding (C-c t)
  ;; (global-set-key (kbd "C-c t") telega-prefix-map)
  )

;; (use-package telega-bubbles
;;   :vc (:url "https://github.com/guidao/telega-bubbles" :rev :newest)
;;   :after telega
;;   :config
;;   (telega-bubbles-mode 1))
(provide 'init-telega)
;;; init-telega.el ends here
