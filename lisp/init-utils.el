;;; init-utils.el --- Utilities -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; discord ipc
(use-package
 elcord
 :ensure t
 :defer t
 :init (setq elcord-quiet t)
 :config (elcord-mode))

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
 :hook (emacs-lisp-mode . elisp-autofmt-mode))
 ; :custom (elisp-autofmt-on-save-p 'always))

;; wakatime
(use-package wakatime-mode :ensure t :defer 5 :config (global-wakatime-mode))

;; better undo
(use-package undo-fu :ensure t)

;; undo session
(use-package
 undo-fu-session
 :ensure t
 :defer t
 :config (undo-fu-session-global-mode))

;; test speed
(use-package benchmark-init
  :ensure t
  :config
  ;; 启动后自动显示报告
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(provide 'init-utils)
;;; init-utils.el ends here
