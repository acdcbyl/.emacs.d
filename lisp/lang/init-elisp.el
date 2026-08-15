;;; init-elisp.el --- Elisp configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . eval-last-sexp-with-comment)
              :map lisp-interaction-mode-map
              ("C-c C-c" . eval-last-sexp-with-comment))
  :config
  (defun eval-last-sexp-with-comment (&optional arg)
    (interactive "P")
    (let ((start (point)))
      (eval-print-last-sexp arg)
      (save-excursion
        (goto-char start)
        (forward-line 1)
        (insert ";;=> ")))))

(provide 'init-elisp)
;;; init-elisp.el ends here
