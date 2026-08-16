;;; init-python.el --- Python configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; NOTE: python-ts-mode lives in the built-in `python' feature; with
;; `treesit-enabled-modes' set to t, .py files reach it through the
;; `(python-mode . python-ts-mode)' remap.  `:after (apheleia eglot)'
;; runs the body once both the formatter (apheleia) and the LSP
;; client (eglot) are loaded, keeping `python' itself lazy.

(use-package python
  :ensure nil
  :after (apheleia eglot)
  :hook (python-ts-mode . eglot-ensure)
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("ty" "server"))))

;; NOTE: hook on python-ts-mode rather than python-mode: with
;; `treesit-enabled-modes' set to t, .py files open in python-ts-mode
;; and only its hook is guaranteed to run (on git Emacs python-mode
;; happens to be a parent of python-ts-mode, but relying on that is
;; fragile across versions).
(use-package flymake-ruff
  :ensure t
  :hook (python-ts-mode . flymake-ruff-load))

;; (use-package lsp-proxy
;;   :ensure nil
;;   :after eglot
;;   :hook (python-ts-mode . lsp-proxy-mode))

(provide 'init-python)
;;; init-python.el ends here
