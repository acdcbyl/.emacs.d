;;; init-yaml.el --- YAML configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode :ensure t :defer t :mode "\.ya?ml\'")
(add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))

(provide 'init-yaml)
;;; init-yaml.el ends here
