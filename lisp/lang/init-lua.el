;;; init-lua.el --- Lua configuration -*- lexical-binding: t -*-
;;; Commentary:
;; No :mode entry: with `treesit-enabled-modes' set to t, startup
;; installs ("\\.lua\\'" . lua-ts-mode-maybe) for the built-in
;; lua-ts-mode already.
;;; Code:

(use-package lua-ts-mode
  :ensure nil
  :after eglot
  :hook (lua-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(lua-ts-mode . ("lua-language-server"))))

(provide 'init-lua)
;;; init-lua.el ends here
