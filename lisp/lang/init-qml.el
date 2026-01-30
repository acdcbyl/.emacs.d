;;; init-qml.el --- QML configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package qml-ts-mode
  :vc (:url "https://github.com/xhcoding/qml-ts-mode"
            :rev :newest)
  :mode "\.qml\'"
  :defer t)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(qml-ts-mode . ("qmlls6")))
  (add-hook 'qml-ts-mode-hook 'eglot-ensure))

(provide 'init-qml)
;;; init-qml.el ends here
