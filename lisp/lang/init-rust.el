;;; init-rust.el --- Rust configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; NOTE: the third-party `rust-mode' package has been removed.  With
;; `treesit-enabled-modes' set to t, .rs files are handled by the
;; built-in rust-ts-mode anyway (the treesit remap redirects
;; rust-mode → rust-ts-mode before rust-mode ever activates, so all
;; of rust-mode's settings were dead code).  Eglot below is what
;; powers IDE features.

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))
  (add-hook 'rust-ts-mode-hook 'eglot-ensure))

;; (with-eval-after-load 'lsp-proxy
;;   (add-hook 'rust-ts-mode-hook #'lsp-proxy-mode))
(provide 'init-rust)
;;; init-rust.el ends here
