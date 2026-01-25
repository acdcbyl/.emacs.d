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
 :commands (mpdel-mode mpdel-song-open)
 :defer t)
;; for embark
(use-package
 mpdel-embark
 :ensure t
 :after (embark mpdel)
 :config
 (progn
   (mpdel-embark-setup)))

(provide 'init-music)
;;; init-music.el ends here
