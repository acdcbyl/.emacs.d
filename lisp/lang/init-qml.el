;;; init-qml.el --- QML configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package qml-ts-mode
  :vc (:url "https://github.com/xhcoding/qml-ts-mode"
            :rev :newest)
  :mode "\\.qml\\'"
  :defer t
  :config
  ;; qml-ts-mode only calls `treesit-ready-p' (which just warns when
  ;; the grammar is missing).  Ask to install it like the built-in
  ;; -ts-modes do.
  (defun aiser/qml-ensure-grammar ()
    (treesit-ensure-installed 'qmljs))
  (advice-add #'qml-ts-mode :before #'aiser/qml-ensure-grammar))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(qml-ts-mode . ("qmlls6")))
  (add-hook 'qml-ts-mode-hook 'eglot-ensure))
;; (with-eval-after-load 'lsp-proxy
;;   (add-hook 'qml-ts-mode-hook #'lsp-proxy-mode))
(provide 'init-qml)
;;; init-qml.el ends here
