;;; init-kdl.el --- KDL configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package kdl-ts-mode
  :vc (:url "https://github.com/merrickluo/kdl-ts-mode" :rev :newest)
  :defer t
  :mode "\\.kdl\\'")

(provide 'init-kdl)
;;; init-kdl.el ends here
