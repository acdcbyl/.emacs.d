;;; init-telega.el --- Telegram for emacs -*- lexical-binding: t no-byte-compile: t-*-
;;; Commentary:
;;; Code:

(use-package telega
  :ensure t
  :commands telega

  :init
  ;; Basic directory paths
  (setq telega-directory "/home/aiser/.local/share/telega/")
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
  (setq telega-root-default-view-function #'telega-view-default)
  (setq telega-root-keep-cursor 'track)
  (setq telega-root-buffer-name "*Telega Root*")

  ;; Hide chat folder icon prefixes for clean layout
  (setopt telega-chat-folders-insexp (lambda () nil))
  (setopt telega-root-fill-column 70)
  (setopt telega-filters-custom nil)
  (setopt telega-filter-custom-show-folders nil)

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

  (setq telega-builtin-palettes-alist
        '((light
           ((:outline "#b4637a") (:foreground "#b4637a") (:background "#d1c7c7"))
           ((:outline "#ea9d34") (:foreground "#ea9d34") (:background "#d1ccc7"))
           ((:outline "#907aa9") (:foreground "#907aa9") (:background "#cdc7d1"))
           ((:outline "#568D68") (:foreground "#568D68") (:background "#c7d1c7"))
           ((:outline "#286983") (:foreground "#286983") (:background "#cacfcf"))
           ((:outline "#56949f") (:foreground "#56949f") (:background "#c6cbd1"))
           ((:outline "#d7827e") (:foreground "#d7827e") (:background "#d1c7cb")))
          (dark
           ((:outline "#eb6f92") (:foreground "#eb6f92") (:background "#3d2828"))
           ((:outline "#f6c177") (:foreground "#f6c177") (:background "#2c2620"))
           ((:outline "#b294bb") (:foreground "#b294bb") (:background "#2e1e2e"))
           ((:outline "#95b1ac") (:foreground "#95b1ac") (:background "#1e2e1e"))
           ((:outline "#81a2be") (:foreground "#81a2be") (:background "#234242"))
           ((:outline "#9ccfd8") (:foreground "#9ccfd8") (:background "#1e262e"))
           ((:outline "#ebbcba") (:foreground "#ebbcba") (:background "#470528")))))

  (with-eval-after-load 'telega
    (custom-set-faces
     '(telega-palette-builtin-green
       ((t (:foreground "#95b1ac"
                        :background "#1e2e1e"))))

     '(telega-palette-builtin-blue
       ((t (:foreground "#81a2be"
                        :background "#234242"))))

     '(telega-palette-builtin-orange
       ((t (:foreground "#f6c177"
                        :background "#2c2620"))))

     '(telega-palette-builtin-purple
       ((t (:foreground "#b294bb"
                        :background "#2e1e2e"))))))
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
    (setopt
     telega-symbols-emojify
     (cl-reduce (lambda (emojify key)
                  (assq-delete-all key emojify))
                '(verified vertical-bar checkmark forum heavy-checkmark reply reply-quote horizontal-bar forward button-close summarize-in summarize-out)
                :initial-value telega-symbols-emojify)
     telega-symbol-button-close (nerd-icons-mdicon "nf-md-close_box_outline")
     telega-symbol-verified (nerd-icons-codicon "nf-cod-verified_filled")
     telega-symbol-vertical-bar "│" ;; U+2502 Box Drawings Light Vertical
     telega-symbol-saved-messages-tag-end (nerd-icons-faicon "nf-fa-tag")
     telega-symbol-forum (nerd-icons-mdicon "nf-md-format_list_text")
     telega-symbol-flames (nerd-icons-mdicon "nf-md-delete_clock")
     telega-symbol-mark (propertize " " 'face 'telega-button-highlight)
     telega-symbol-reply (nerd-icons-faicon "nf-fa-reply")
     telega-symbol-reply-quote (nerd-icons-faicon "nf-fa-reply_all")
     telega-symbol-forward (nerd-icons-faicon "nf-fa-mail_forward")
     telega-symbol-checkmark (nerd-icons-mdicon "nf-md-check")
     telega-symbol-heavy-checkmark (nerd-icons-codicon "nf-cod-check_all")
     telega-symbol-summarize-in (nerd-icons-octicon "nf-oct-fold")
     telega-symbol-summarize-out (nerd-icons-octicon "nf-oct-unfold")))
  )

(provide 'init-telega)
;;; init-telega.el ends here
