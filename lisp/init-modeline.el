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
  (lambda-line-prefix nil)
  (lambda-line-icon-time t)
  (lambda-line-prefix-padding nil)
  (lambda-line-status-invert nil)
  (lambda-line-vc-symbol "⎇ ")
  (lambda-line-space-top +.25)
  (lambda-line-space-bottom -.25)
  (lambda-line-symbol-position 0.1)
  (lambda-line-word-count-enabled t)
  :custom-face
  (lambda-line-visual-bell
   ((t (:background ,(doom-color 'red)))))
  :config
  (lambda-line-mode)
  (lambda-line-visual-bell-config)

  (defun my/lambda-line-clockface-setup ()
    "Setup ClockFace font on frame fontset with higher priority than nerd-icons."
    (let ((font "ClockFaceRect")
          (range (cons (decode-char 'ucs #xF0000)
                       (decode-char 'ucs #xF008F))))
      (set-fontset-font "fontset-default" range (font-spec :family font) nil 'prepend)
      (when (display-graphic-p)
        (dolist (frame (frame-list))
          (set-fontset-font (frame-parameter frame 'font) range
                            (font-spec :family font) frame 'prepend)))))

  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_"))))

(use-package minions
  :ensure t
  :config (minions-mode 1))

(use-package
  hide-mode-line
  :ensure t
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
     mpdel-browser-mode
     mpdel-tablist-mode
     mpdel-playlist-mode
     mpdel-song-mode
     lsp-ui-imenu-mode
     pdf-view-mode
     pdf-annot-list-mode)
    . turn-on-hide-mode-line-mode)))

(provide 'init-modeline)
;;; init-modeline.el ends here
