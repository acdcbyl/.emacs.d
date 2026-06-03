;;; init-tramp-rpc.el --- Make tramp great again -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package
  msgpack
  :ensure t)
(use-package tramp-rpc
  :after tramp
  :defer t
  :vc (:url "https://github.com/ArthurHeymans/emacs-tramp-rpc"
            :rev :newest
            :lisp-dir "lisp"))
(provide 'init-tramp-rpc)
;;; init-tramp-rpc.el ends here.
