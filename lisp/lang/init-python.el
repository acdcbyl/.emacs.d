;;; init-python.el --- Python configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'apheleia
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff)))

(use-package flymake-ruff
  :ensure t
  :hook (python-mode . flymake-ruff-load))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("ty" "server")))
  (add-hook 'python-ts-mode-hook 'eglot-ensure))

;; (with-eval-after-load 'lsp-proxy
;;   (add-hook 'python-ts-mode-hook #'lsp-proxy-mode))
(provide 'init-python)
;;; init-python.el ends here
