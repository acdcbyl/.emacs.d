;;; init-ui.el --- Tab bar and minor UI tweaks -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Colorize color names in buffers
(use-package
  colorful-mode
  :ensure t
  :diminish
  ;; Enable per buffer via hooks instead of `global-colorful-mode':
  ;; scanning for color names is wasteful in large non-code buffers.
  :init (setq colorful-use-prefix t)
  :hook ((prog-mode text-mode css-mode html-mode php-mode help-mode helpful-mode)
         . colorful-mode))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters :ensure t :hook prog-mode)

;; More features help
(use-package
  helpful
  :ensure t
  :defer t)


(provide 'init-ui)
;;; init-ui.el ends here
