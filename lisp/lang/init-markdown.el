;;; init-markdown.el --- Markdown configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("README\.md\'" . gfm-mode)
  :hook ((markdown-mode . visual-line-mode)))

(use-package grip-mode
  :ensure t
  :defer t
  :config
  (setq grip-command 'go-grip) ;; auto, grip, go-grip or mdopen
  ;; :hook ((markdown-mode org-mode) . grip-mode)
  )

(provide 'init-markdown)
;;; init-markdown.el ends here
