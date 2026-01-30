;;; init-js.el --- JavaScript/TypeScript configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package js2-mode :ensure t)

(add-to-list 'major-mode-remap-alist '(js2-mode . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))

(provide 'init-js)
;;; init-js.el ends here
