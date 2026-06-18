;;; init-js.el --- JavaScript/TypeScript configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package js2-mode :ensure t)

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
