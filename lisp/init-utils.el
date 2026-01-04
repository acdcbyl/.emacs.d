;;; init-utils.el --- Utilities -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; discord ipc
(use-package elcord
  :ensure t
  :custom
  (elcord-display-buffer-name t)
  (elcord-display-line-number t)
  (elcord-update-interval 60)

  :config
  ;; ---------- 自动检测 Discord 是否运行 ----------
  (defun my/discord-running-p ()
    "Return t if Discord process is running."
    (or
     ;; 方法1：检查进程列表（Linux/macOS/Windows 都适用）
     (seq-some (lambda (proc)
                 (string-match-p "Vesktop" (process-name proc)))
               (process-list))
     ;; 方法2：如果 Discord 可执行文件在 PATH 中（可选）
     ;; (executable-find "Discord")
     ))

  (defvar my/elcord-check-timer nil
    "Timer for periodically checking Discord status.")

  (defun my/elcord-auto-control ()
    "Enable elcord-mode only when Discord is running."
    (if (my/discord-running-p)
        (unless elcord-mode
          (elcord-mode 1)
          (message "Discord detected → elcord-mode enabled"))
      (when elcord-mode
        (elcord-mode -1)
        (message "Discord not running → elcord-mode disabled"))))

  ;; 每 30 秒检查一次（可根据需要调整）
  (setq my/elcord-check-timer
        (run-with-timer 0 30 #'my/elcord-auto-control))

  ;; Emacs 关闭时取消定时器（好习惯，避免警告）
  (add-hook 'kill-emacs-hook
            (lambda ()
              (when my/elcord-check-timer
                (cancel-timer my/elcord-check-timer)))))

;; make elisp-autofmt
(use-package elisp-autofmt
  :ensure t
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

(provide 'init-utils)
;;; init-utils.el ends here
