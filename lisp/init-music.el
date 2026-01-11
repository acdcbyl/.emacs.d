;;; init-music.el --- Bring music play for emacs -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Core Packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package
 mpdel
 :ensure t
 ;; :defer t
 ;; :init
 ;; (with-eval-after-load 'mpdel
 ;;   (evil-collection-mpdel-setup))
 :config (mpdel-mode 1))
;; for embark
(use-package
 mpdel-embark
 :ensure t
 :demand t
 :after (embark mpdel)
 :config
 (progn
   (mpdel-embark-setup)))

(provide 'init-music)
;;; init-music.el ends here
