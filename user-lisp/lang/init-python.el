;;; init-python.el --- Python configuration -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(use-package python
  :ensure nil
  :hook (python-ts-mode . eglot-ensure))

;; Register the LSP server as soon as eglot loads, NOT inside a deferred
;; `:after' body: if eglot was already loaded (e.g. by another language's
;; `eglot-ensure') before this file runs, the deferred registration could
;; race or be skipped entirely and eglot would silently find no server.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("ty" "server"))))

;; apheleia formatter mapping: only meaningful once both features exist.
(with-eval-after-load 'apheleia
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff)))

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
