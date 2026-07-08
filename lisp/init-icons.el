;;; init-icons.el --- nerd-icons -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

;; Use nerd-icons as the icon package
(use-package nerd-icons
  :ensure t
  :demand t
  :config
  (setq nerd-icons-scale-factor 0.8)
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

;; Adapt icons for other windows
(use-package
  nerd-icons-completion
  :ensure t
  :after (marginalia nerd-icons)
  :config (nerd-icons-completion-mode 1)
  (add-hook
   'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Set icons for ibuffer
(use-package
  nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(provide 'init-icons)
;;; init-icons.el ends here.
