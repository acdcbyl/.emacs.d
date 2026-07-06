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
  (my/lambda-line-clockface-setup)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (when (display-graphic-p frame)
                (let ((range (cons (decode-char 'ucs #xF0000)
                                   (decode-char 'ucs #xF008F))))
                  (set-fontset-font (frame-parameter frame 'font) range
                                    (font-spec :family "ClockFaceRect") frame 'prepend)))))

  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_")))

  (defgroup aiser-modeline nil
    "Matugen modeline extensions."
    :group 'lambda-line)

  (with-eval-after-load 'evil
    (defface modeline-evil-normal
      `((t (:background ,(doom-color 'blue) :foreground ,(doom-color 'blue-fg) :weight bold
                        :box (:line-width 1 :color ,(doom-color 'blue) :style nil))))
      "Face for the evil normal-state tag in lambda-line."
      :group 'aiser-modeline)

    (defface modeline-evil-insert
      `((t (:background ,(doom-color 'green) :foreground ,(doom-color 'green-fg) :weight bold
                        :box (:line-width 1 :color ,(doom-color 'green) :style nil))))
      "Face for the evil insert-state tag in lambda-line."
      :group 'aiser-modeline)

    (defface modeline-evil-visual
      `((t (:background ,(doom-color 'pink) :foreground ,(doom-color 'pink-fg) :weight bold
                        :box (:line-width 1 :color ,(doom-color 'pink) :style nil))))
      "Face for the evil visual-state tag in lambda-line."
      :group 'aiser-modeline)

    (defface modeline-evil-visual-line
      `((t (:background ,(doom-color 'pink) :foreground ,(doom-color 'pink-fg) :weight bold
                        :box (:line-width 1 :color ,(doom-color 'pink) :style nil))))
      "Face for the evil visual-line-state tag in lambda-line."
      :group 'aiser-modeline)

    (defface modeline-evil-visual-block
      `((t (:background ,(doom-color 'pink) :foreground ,(doom-color 'pink-fg) :weight bold
                        :box (:line-width 1 :color ,(doom-color 'pink) :style nil))))
      "Face for the evil visual-block-state tag in lambda-line."
      :group 'aiser-modeline)

    (defface modeline-evil-replace
      `((t (:background ,(doom-color 'red) :foreground ,(doom-color 'red-fg) :weight bold
                        :box (:line-width 1 :color ,(doom-color 'red) :style nil))))
      "Face for the evil replace-state tag in lambda-line."
      :group 'aiser-modeline)

    (defface modeline-evil-emacs
      `((t (:background ,(doom-color 'blue) :foreground ,(doom-color 'blue-fg) :weight bold
                        :box (:line-width 1 :color ,(doom-color 'blue) :style nil))))
      "Face for the evil emacs-state tag in lambda-line."
      :group 'aiser-modeline)

    (defface modeline-evil-motion
      `((t (:background ,(doom-color 'yellow) :foreground ,(doom-color 'yellow-fg) :weight bold
                        :box (:line-width 1 :color ,(doom-color 'yellow) :style nil))))
      "Face for the evil motion-state tag in lambda-line."
      :group 'aiser-modeline)

    (defface modeline-evil-operator
      `((t (:background ,(doom-color 'yellow) :foreground ,(doom-color 'yellow-fg) :weight bold
                        :box (:line-width 1 :color ,(doom-color 'yellow) :style nil))))
      "Face for the evil operator-state tag in lambda-line."
      :group 'aiser-modeline)

    (with-eval-after-load 'lambda-line
      (dolist (pair '((lambda-line-evil-normal       . modeline-evil-normal)
                      (lambda-line-evil-insert       . modeline-evil-insert)
                      (lambda-line-evil-visual       . modeline-evil-visual)
                      (lambda-line-evil-visual-line  . modeline-evil-visual-line)
                      (lambda-line-evil-visual-block . modeline-evil-visual-block)
                      (lambda-line-evil-replace      . modeline-evil-replace)
                      (lambda-line-evil-emacs        . modeline-evil-emacs)
                      (lambda-line-evil-motion       . modeline-evil-motion)
                      (lambda-line-evil-operator     . modeline-evil-operator)))
        (set-face-attribute (car pair) nil
                            :foreground 'unspecified
                            :background 'unspecified
                            :inherit (cdr pair))))))

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
