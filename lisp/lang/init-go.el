;;; init-go.el --- Go configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; NOTE: the third-party `go-mode' package has been removed.  With
;; `treesit-enabled-modes' set to t, .go files are handled by the
;; built-in go-ts-mode anyway (remap `(go-mode . go-ts-mode)'), and
;; go-mode's go-dot-mod-mode / go-dot-work-mode shadowed the built-in
;; go-mod-ts-mode / go-work-ts-mode for go.mod / go.work files.

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(go-ts-mode . ("gopls" :initializationOptions
                               (:hints (:assignVariableTypes t
                                                             :compositeLiteralFields t
                                                             :parameterNames t
                                                             :functionTypeParameters t)))))
  (add-hook 'go-ts-mode-hook 'eglot-ensure))

;; (with-eval-after-load 'lsp-proxy
;;   (add-hook 'go-ts-mode-hook #'lsp-proxy-mode))
(provide 'init-go)
;;; init-go.el ends here
