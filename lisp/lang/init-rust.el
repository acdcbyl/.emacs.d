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

(use-package flycheck-rust
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'rust-ts-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))
  (add-hook 'rust-ts-mode-hook 'eglot-ensure))

(provide 'init-rust)
;;; init-rust.el ends here
