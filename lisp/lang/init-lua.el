;;; init-lua.el --- Lua configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lua-ts-mode
  :ensure nil
  :mode "\\.lua\\'"
  :after eglot
  :hook (lua-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(lua-ts-mode . ("lua-language-server"))))

(provide 'init-lua)
;;; init-lua.el ends here
