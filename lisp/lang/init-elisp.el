;;; init-elisp.el --- Elisp configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package
  flycheck-package
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(flycheck-package-setup)))

(provide 'init-elisp)
;;; init-elisp.el ends here
