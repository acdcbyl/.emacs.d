;;; init-dap.el --- modeline,dashboard and icons -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
;; Left and right side windows occupy full frame height
(use-package emacs :custom (window-sides-vertical t))

;; For a more ergonomic Emacs and `dape' experience
(use-package repeat :custom (repeat-mode +1))

(use-package
 dape
 :ensure t
 :bind ("<f5>" . dape)
 :custom (dape-buffer-window-arrangment 'right)
 :config
 ;; Save buffers on startup, useful for interpreted languages
 (add-hook 'dape-start-hook (lambda () (save-some-buffers t t))))

(provide 'init-dap)
;;; init-dap.el ends here
