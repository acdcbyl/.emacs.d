;;; init-theme.el --- Theme and visual environment -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(setq window-divider-default-right-width 10)
(set-frame-parameter nil 'internal-border-width 15)
(window-divider-mode 1)

(use-package
  doom-themes
  ;; :load-path "doom-themes-matugen"
  :vc (:url "https://github.com/acdcbyl/doom-themes-matugen" :rev :newest)
  :custom (doom-themes-enable-bold t)
  (doom-matugen-region-highlight 'frost)
  (doom-themes-enable-italic t)
  :config (load-theme 'doom-matugen t)
  (doom-themes-org-config)
  )

(provide 'init-theme)
;;; init-theme.el ends here
