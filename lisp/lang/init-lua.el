;;; init-lua.el --- Lua configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package
 lua-mode
 :ensure t
 :mode "\\.lua\\'"
 :defer t)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(lua-ts-mode . ("lua-language-server")))
  (add-hook 'lua-ts-mode-hook 'eglot-ensure))

(provide 'init-lua)
;;; init-lua.el ends here
