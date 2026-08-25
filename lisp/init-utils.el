;;; init-utils.el --- Utilities -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; discord ipc
(use-package
  elcord
  :ensure t
  :defer 6
  :init (setq elcord-quiet t)
  :config (elcord-mode))

;; wakatime
(use-package wakatime-mode :ensure t :defer 5 :config (global-wakatime-mode))

;; better undo
(use-package undo-fu :ensure t)

;; undo session
(use-package undo-fu-session
  :ensure t
  :after undo-fu
  :defer 1
  :config (undo-fu-session-global-mode 1))

(defun +copy-current-buffer-name ()
  "Copy the current buffer's name to the kill ring."
  (interactive)
  (kill-new (buffer-name))
  (message "Copied buffer name: %s" (buffer-name)))

(provide 'init-utils)
;;; init-utils.el ends here
