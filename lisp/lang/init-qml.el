;;; init-qml.el --- QML configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package qml-ts-mode
  :vc (:url "https://github.com/xhcoding/qml-ts-mode"
            :rev :newest)
  ;; :mode is registered centrally in init-dev.el (this file is only
  ;; loaded once the mode library is first used).
  :after eglot
  :hook (qml-ts-mode . eglot-ensure)
  :config
  ;; qml-ts-mode only calls `treesit-ready-p' (which just warns when
  ;; the grammar is missing).  Ask to install it like the built-in
  ;; -ts-modes do.
  (defun aiser/qml-ensure-grammar ()
    (treesit-ensure-installed 'qmljs))
  (advice-add #'qml-ts-mode :before #'aiser/qml-ensure-grammar)
  (add-to-list 'eglot-server-programs '(qml-ts-mode . ("qmlls6"))))

;; (use-package lsp-proxy
;;   :ensure nil
;;   :after eglot
;;   :hook (qml-ts-mode . lsp-proxy-mode))

(provide 'init-qml)
;;; init-qml.el ends here
