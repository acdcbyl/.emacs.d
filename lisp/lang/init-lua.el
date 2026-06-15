;;; init-lua.el --- Lua configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package
  lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :defer t)

(add-to-list 'major-mode-remap-alist '(lua-mode . lua-ts-mode))


(with-eval-after-load 'lsp-proxy
  (add-hook 'lua-ts-mode-hook #'lsp-proxy-mode))

(provide 'init-lua)
;;; init-lua.el ends here
