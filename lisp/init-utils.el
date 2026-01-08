;;; init-utils.el --- Utilities -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; discord ipc
(use-package
 elcord
 :ensure t
 :init (elcord-mode)
 :config (setq elcord-quiet t))

;; Set up good scroll
;; (use-package good-scroll
;;  :ensure t
;;  :if window-system     ; Only use this plugin in graphical interface
;;  :init (good-scroll-mode))

;; (use-package sublimity
;;   :ensure t
;;   :config
;;   (require 'sublimity-scroll)
;;   ;; (require 'sublimity-map)   ;Minimap function
;;   ;; (require 'sublimity-attractive)  ;Cursor effect
;;   (sublimity-mode 1))
;; Set Chinese font
;; (use-package cnfonts
;;   :ensure t
;;   :init (cnfonts-mode 1))

;; make elisp-autofmt
(use-package
 elisp-autofmt
 :ensure t
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode)
 :custom (elisp-autofmt-on-save-p 'always))

;; wakatime
(use-package wakatime-mode :ensure t :config (global-wakatime-mode))

;; better undo
(use-package undo-fu :ensure t)

;; undo session
(use-package
 undo-fu-session
 :ensure t
 :config (undo-fu-session-global-mode))

(provide 'init-utils)
;;; init-utils.el ends here
