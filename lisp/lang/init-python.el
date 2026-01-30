;;; init-python.el --- Python configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(with-eval-after-load 'apheleia
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("ty" "server")))
  (add-hook 'python-ts-mode-hook 'eglot-ensure))

(provide 'init-python)
;;; init-python.el ends here
