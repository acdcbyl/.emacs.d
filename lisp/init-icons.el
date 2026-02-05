;;; init-icons.el --- nerd-icons -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

;; Use nerd-icons as the icon package
(use-package nerd-icons :ensure t :when (display-graphic-p) :demand t)

;; Adapt icons for other windows
(use-package
  nerd-icons-completion
  :ensure t
  :after (marginalia nerd-icons)
  :config (nerd-icons-completion-mode 1)
  (add-hook
   'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Set icons for dired
;; (use-package
;;  nerd-icons-dired
;;  :ensure t
;;  :hook (dired-mode . nerd-icons-dired-mode))

;; Set icons for ibuffer
(use-package
  nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(provide 'init-icons)
;;; init-icons.el ends here.
