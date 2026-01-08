;;; init-utils.el --- Utilities -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; discord ipc
(use-package elcord
  :ensure t
  :init (elcord-mode)
  :config
  (setq elcord-quiet t))

;; 设置good scroll
;; (use-package good-scroll
;;  :ensure t
;;  :if window-system     ; 在图形化界面时才使用这个插件
;;  :init (good-scroll-mode))

(use-package sublimity
  :ensure t
  :config
  (require 'sublimity-scroll)
  ;; (require 'sublimity-map)   ;小地图功能
  ;; (require 'sublimity-attractive)  ;光标效果
  (sublimity-mode 1))
;; 设置中文字体
;; (use-package cnfonts
;;   :ensure t
;;   :init (cnfonts-mode 1))

;; make elisp-autofmt
(use-package elisp-autofmt
  :ensure t
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

(provide 'init-utils)
;;; init-utils.el ends here
