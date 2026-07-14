;;; init-theme.el --- Theme and visual environment -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; (setq window-divider-default-right-width 30)
;; (set-face-attribute 'header-line nil :box '(:line-width 2 :color nil))
;; (set-face-attribute 'tab-bar-tab nil :box '(:line-width 2 :color nil))
;; (set-face-attribute 'custom-button nil :box '(:line-width 3 :color nil))
(set-frame-parameter nil 'internal-border-width 15)
;; (window-divider-mode 1)

(use-package
  doom-themes
  ;; :load-path "doom-themes-matugen"
  :vc (:url "https://github.com/acdcbyl/doom-themes-matugen" :rev :newest)
  :custom (doom-themes-enable-bold t) (doom-themes-enable-italic t)
  :config (load-theme 'doom-matugen t)
  (doom-themes-org-config)
  )

(provide 'init-theme)
;;; init-theme.el ends here
