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

;; Set up ultra scroll
(use-package ultra-scroll
  :ensure t
  :when (fboundp 'pixel-scroll-precision-mode)
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))

;; make elisp-autofmt
;; (use-package
;;  elisp-autofmt
;;  :ensure t
;;  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
;;  :hook (emacs-lisp-mode . elisp-autofmt-mode))
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
;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; 启动后自动显示报告
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))
;; (use-package which-key-posframe
;;   :ensure t
;;   :diminish
;;   :defines posframe-border-width
;;   :custom-face
;;   (which-key-posframe-border ((t (:inherit posframe-border :background unspecified))))
;;   :hook which-key-mode
;;   :init
;;   (setq which-key-posframe-border-width posframe-border-width
;;         which-key-posframe-poshandler 'posframe-poshandler-frame-center-near-bottom
;;         which-key-posframe-parameters '((left-fringe . 8)
;;                                         (right-fringe . 8))))

(provide 'init-utils)
;;; init-utils.el ends here
