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

;; Undo session persistence: serializes the standard `buffer-undo-list'
;; to disk and restores it on revisit.  Works with the built-in
;; `undo-redo' (see `evil-undo-system' in init-evil.el); no need for the
;; `undo-fu' wrapper package anymore.
(use-package undo-fu-session
  :ensure t
  :defer 1
  :config (undo-fu-session-global-mode 1))

(defun +copy-current-buffer-name ()
  "Copy the current buffer's name to the kill ring."
  (interactive)
  (kill-new (buffer-name))
  (message "Copied buffer name: %s" (buffer-name)))

(provide 'init-utils)
;;; init-utils.el ends here
