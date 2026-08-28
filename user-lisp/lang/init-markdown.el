;;; init-markdown.el --- Markdown configuration -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;; With `treesit-enabled-modes' set to t (see init-dev.el), startup
;; installs the ("\\.md\\'" . markdown-ts-mode-maybe) entry for the
;; built-in markdown-ts-mode, so no explicit :mode registration is
;; needed here (and one would only risk shadowing the built-in one).
;;; Code:

(use-package markdown-ts-mode
  :ensure nil
  :defer t
  :custom
  (markdown-ts-inline-images t)
  (markdown-ts-image-max-width 800)
  (markdown-ts-default-folding 'hide-children)
  :config
  (setq markdown-ts-code-block-modes
        '((el emacs-lisp-mode) (elisp emacs-lisp-mode)
          (sh sh-mode) (bash sh-mode)
          (bat bat-mode) (powershell sh-mode) (vbs js-mode)
          (javascript js-mode) (js js-mode) (jsx js-mode)
          (typescript typescript-ts-mode) (ts typescript-ts-mode) (tsx typescript-tsx-mode)
          (java java-mode) (go go-ts-mode) (rust rust-ts-mode) (python python-mode)
          (c c-ts-mode) (cpp cpp-ts-mode))))

(use-package grip-mode
  :ensure t
  :defer t
  :config
  (setq grip-command 'go-grip) ;; auto, grip, go-grip or mdopen
  ;; :hook ((markdown-mode org-mode) . grip-mode)
  )

(provide 'init-markdown)
;;; init-markdown.el ends here
