;;; init-tg.el --- Bring telegram for emacs -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Core Packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; need tdlib
(use-package telega
  :ensure t
  :commands (telega)
  :defer t)

(provide 'init-tg)

;; init-tg ends here
