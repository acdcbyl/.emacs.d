;;; init-js.el --- JavaScript/TypeScript configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; NOTE: js2-mode is intentionally NOT loaded here: with
;; `treesit-enabled-modes' set to t, .js files are remapped to
;; `js-ts-mode' anyway (the installed js2-mode package no longer even
;; registers itself in `auto-mode-alist'), so js2-mode would only
;; waste startup time.  Use js-ts-mode (powered by tree-sitter) and
;; eglot below.  js-ts-mode lives in the built-in `js' feature while
;; typescript-ts-mode / tsx-ts-mode live in the built-in
;; `typescript-ts-mode' feature; `:after (apheleia eglot)' runs the
;; body once both the formatter (apheleia) and the LSP client (eglot)
;; are loaded, keeping them lazy.

(use-package js
  :ensure nil
  :after (apheleia eglot)
  :hook ((js-ts-mode typescript-ts-mode tsx-ts-mode) . eglot-ensure)
  :config
  (dolist (mode '(js-ts-mode typescript-ts-mode tsx-ts-mode))
    (setf (alist-get mode apheleia-mode-alist) 'prettier))
  (add-to-list 'eglot-server-programs
               '((js-ts-mode typescript-ts-mode tsx-ts-mode)
                 .
                 ("typescript-language-server" "--stdio"))))

;; (use-package lsp-proxy
;;   :ensure nil
;;   :after eglot
;;   :hook ((js-ts-mode typescript-ts-mode tsx-ts-mode) . lsp-proxy-mode))

(provide 'init-js)
;;; init-js.el ends here
