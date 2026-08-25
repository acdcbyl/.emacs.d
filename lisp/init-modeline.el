;;; init-modeline.el --- Mode line configuration -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package lambda-line
  :vc (:url "https://codeberg.org/Lambda-Emacs/lambda-line" :rev :newest)
  :custom
  (lambda-line-abbrev t)
  (lambda-line-position 'bottom)
  (lambda-line-hspace "  ")
  (lambda-line-prefix t)
  (lambda-line-icon-time t)
  (lambda-line-use-nerd-icons t)
  (lambda-line-position-format "%l:%c")
  (lambda-line-prefix-padding nil)
  (lambda-line-status-invert nil)
  (lambda-line-gui-ro-symbol  " ") ;; symbols
  (lambda-line-gui-mod-symbol " ")
  (lambda-line-gui-rw-symbol  " ")
  (lambda-line-vc-symbol "⎇ ")
  (lambda-line-space-top +.25)
  (lambda-line-space-bottom -.25)
  (lambda-line-symbol-position 0.1)
  :custom-face
  (lambda-line-visual-bell
   ((t (:background ,(doom-color 'red)))))
  :config
  ;; MUST be installed before (lambda-line-mode): that call queues an
  ;; advice that hijacks mu4e's header line to draw lambda-line at the
  ;; top of mu4e buffers.  The bottom mode-line already shows lambda-line,
  ;; so neutralise it (the headers view is rendered by mu4e-nano anyway).
  (advice-add 'lambda-line-mu4e-activate :override #'ignore)
  (lambda-line-mode)
  (lambda-line-clockface-update-fontset "ClockFace")
  (customize-set-variable 'flymake-mode-line-counter-format '("" flymake-mode-line-error-counter flymake-mode-line-warning-counter flymake-mode-line-note-counter ""))
  (customize-set-variable 'flymake-mode-line-format '(" " flymake-mode-line-exception flymake-mode-line-counters))
  (with-eval-after-load 'flymake
    (set-face-attribute 'flymake-error-echo nil
                        :foreground (doom-color 'red)
                        :weight 'bold)

    (set-face-attribute 'flymake-warning-echo nil
                        :foreground (doom-color 'yellow)
                        :weight 'bold)

    (set-face-attribute 'flymake-note-echo nil
                        :foreground (doom-color 'green)))
  ;; set divider line in footer
  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_")))
  )

;; Emacs 31: built-in buffer-local minor mode that hides the current
;; buffer's mode line -- replaces the third-party `hide-mode-line'
;; package (which worked by the same trick of setting `mode-line-format').
(use-package mode-line-invisible
  :ensure nil
  :hook
  (((eat-mode
     eshell-mode
     shell-mode
     term-mode
     vterm-mode
     helpful-mode
     embark-collect-mode
     quickrun--mode
     ghostel-mode
     emms-browser-mode
     emms-playlist-mode
     emms-mark-mode
     emms-stream-mode
     emms-ui-albums-mode
     emms-ui-list-mode
     emms-ui-now-playing-mode
     symbols-outline-mode
     lsp-ui-imenu-mode
     pdf-view-mode
     pdf-annot-list-mode)
    . mode-line-invisible-mode)))

(provide 'init-modeline)
;;; init-modeline.el ends here
