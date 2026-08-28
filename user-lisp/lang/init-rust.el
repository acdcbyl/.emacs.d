;;; init-rust.el --- Rust configuration -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; NOTE: the third-party `rust-mode' package has been removed.  With
;; `treesit-enabled-modes' set to t, .rs files are handled by the
;; built-in rust-ts-mode directly: on git Emacs (31+) the
;; auto-mode-alist entry `("\\.rs\\'" . rust-ts-mode-maybe)' plus the
;; remap `(rust-mode . rust-ts-mode)' take care of it, so all of
;; rust-mode's settings would be dead code.  Eglot below is what
;; powers IDE features.

(use-package rust-ts-mode
  :ensure nil
  :after eglot
  :hook (rust-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer"))))

;; (use-package lsp-proxy
;;   :ensure nil
;;   :after eglot
;;   :hook (rust-ts-mode . lsp-proxy-mode))

(provide 'init-rust)
;;; init-rust.el ends here
