;;; init-go.el --- Go configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; NOTE: the third-party `go-mode' package has been removed.  With
;; `treesit-enabled-modes' set to t, .go files are handled by the
;; built-in go-ts-mode anyway: on git Emacs (31+) the auto-mode-alist
;; entry `("\\.go\\'" . go-ts-mode-maybe)' plus the remap
;; `(go-mode . go-ts-mode)' take care of it.  go-mode's
;; go-dot-mod-mode / go-dot-work-mode would also shadow the built-in
;; go-mod-ts-mode / go-work-ts-mode for go.mod / go.work files, so it
;; is deliberately not installed.

;; Emacs 31: built-in unit-test commands in go-ts-mode:
;;   C-c C-t t  run test at point (all tests under active region)
;;   C-c C-t f  run all tests in current file
;;   C-c C-t p  run all tests in current package
(use-package go-ts-mode
  :ensure nil
  :after eglot
  :hook (go-ts-mode . eglot-ensure)
  :custom
  ;; Extra flags for `go test'; add build tags here if needed, e.g.
  ;; '("-tags=integration" "-count=1")
  (go-ts-mode-test-flags nil)
  :config
  (add-to-list 'eglot-server-programs
               '(go-ts-mode . ("gopls" :initializationOptions
                               (:hints (:assignVariableTypes t
                                                             :compositeLiteralFields t
                                                             :parameterNames t
                                                             :functionTypeParameters t))))))

;; (use-package lsp-proxy
;;   :ensure nil
;;   :after eglot
;;   :hook (go-ts-mode . lsp-proxy-mode))

(provide 'init-go)
;;; init-go.el ends here
