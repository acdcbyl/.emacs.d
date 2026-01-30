;;; init-go.el --- Go configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package go-mode :ensure t :defer t)

(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(go-ts-mode . ("gopls" :initializationOptions
                               (:hints (:assignVariableTypes t
                                                             :compositeLiteralFields t
                                                             :parameterNames t
                                                             :functionTypeParameters t)))))
  (add-hook 'go-ts-mode-hook 'eglot-ensure))

(provide 'init-go)
;;; init-go.el ends here
