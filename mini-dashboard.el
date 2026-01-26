;; minimal-dashboard.el

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; 防止重复安装
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; 只装这两个包
(use-package page-break-lines
  :config
  (global-page-break-lines-mode 1))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)

  ;; 可选：想更干净可以关掉一些东西
  ;; (setq dashboard-items '((recents  . 5)
  ;;                         (projects . 5)))
  ;; (setq dashboard-set-heading-icons nil)
  ;; (setq dashboard-set-file-icons nil)
  ;; (setq dashboard-set-navigator nil)

  ;; 让 Emacs 一启动就显示 dashboard
  ; (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))
