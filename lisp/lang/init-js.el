;;; init-js.el --- JavaScript/TypeScript configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; NOTE: js2-mode is intentionally NOT loaded here: with
;; `treesit-enabled-modes' set to t, .js files are remapped to
;; `js-ts-mode' anyway (the installed js2-mode package no longer even
;; registers itself in `auto-mode-alist'), so js2-mode would only
;; waste startup time.  Use js-ts-mode (powered by tree-sitter) and
;; eglot below.

(with-eval-after-load 'apheleia
  (dolist (mode '(js-ts-mode typescript-ts-mode tsx-ts-mode))
    (setf (alist-get mode apheleia-mode-alist)
          'prettier)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((js-ts-mode typescript-ts-mode tsx-ts-mode)
                 .
                 ("typescript-language-server" "--stdio")))
  (dolist (hook '(js-ts-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook))
    (add-hook hook #'eglot-ensure)))

(provide 'init-js)
;;; init-js.el ends here
