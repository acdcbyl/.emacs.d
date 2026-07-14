;;; init-rust.el --- Rust configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rust-mode
  :ensure t
  :defer t
  :init (setq rust-mode-treesitter-derive t)
  :custom
  (rust-indent-where-clause t)
  (rust-load-optional-libraries t))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))
  (add-hook 'rust-ts-mode-hook 'eglot-ensure))

;; (with-eval-after-load 'lsp-proxy
;;   (add-hook 'rust-ts-mode-hook #'lsp-proxy-mode))
(provide 'init-rust)
;;; init-rust.el ends here
