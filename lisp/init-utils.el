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

;; make elisp-autofmt
(use-package elisp-autofmt
  :ensure t
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

(provide 'init-utils)
;;; init-utils.el ends here
