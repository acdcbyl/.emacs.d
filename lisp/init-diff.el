;;; init-diff.el --- Diff highlighting in fringe -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package diff-hl
  :ensure t
  :defines diff-hl-show-hunk-posframe-internal-border-color
  :commands (diff-hl-flydiff-mode diff-hl-margin-mode)
  :custom-face
  (diff-hl-change ((t (:inherit custom-changed :foreground unspecified :background unspecified))))
  (diff-hl-insert ((t (:inherit diff-added :background unspecified))))
  (diff-hl-delete ((t (:inherit diff-removed :background unspecified))))
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (after-load-theme . diff-hl-set-posframe-appearance)
         (dired-mode . diff-hl-dired-mode))
  :custom
  (diff-hl-draw-borders nil)
  (diff-hl-update-async t)
  (diff-hl-global-modes '(not image-mode pdf-view-mode))
  (diff-hl-show-hunk-function (if (childframe-workable-p)
                                  'diff-hl-show-hunk-posframe
                                'diff-hl-show-hunk-inline))
  :init
  (defun diff-hl-set-posframe-appearance ()
    "Set appearance of diff-hl-posframe."
    (setq diff-hl-show-hunk-posframe-internal-border-color
          (face-background 'posframe-border nil t)))
  (diff-hl-set-posframe-appearance)
  :config
  (setq-default fringes-outside-margins t)

  (defun my-diff-hl-fringe-bmp-function (_type _pos)
    "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
    (define-fringe-bitmap 'my-diff-hl-bmp
      (vector #b11111100)
      1 8
      '(center t)))
  (setq diff-hl-fringe-bmp-function 'my-diff-hl-fringe-bmp-function)

  (diff-hl-flydiff-mode 1)

  (unless (display-graphic-p) (diff-hl-margin-mode 1)))

(provide 'init-diff)
;;; init-diff.el ends here
