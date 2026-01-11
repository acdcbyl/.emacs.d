;;; init-package.el --- If you can't get package from MELPA	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; quela configuration.
;;

;;; Code:

;; Bootstrap `quelpa'.
(use-package quelpa
  :ensure t
  :commands quelpa
  :custom
  (quelpa-git-clone-depth 1)
  (quelpa-self-upgrade-p nil)
  (quelpa-update-melpa-p nil)
  (quelpa-checkout-melpa-p nil))

(provide 'init-package)
;;; init-package.el ends here
