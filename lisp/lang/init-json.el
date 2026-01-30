;;; init-json.el --- JSON configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package json-mode :ensure t :defer t)
(add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))

(provide 'init-json)
;;; init-json.el ends here
