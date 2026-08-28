;;; init-kdl.el --- KDL configuration -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(use-package kdl-ts-mode
  :vc (:url "https://github.com/merrickluo/kdl-ts-mode" :rev :newest)
  :defer t
  ;; :mode is registered centrally in init-dev.el (this file is only
  ;; loaded once the mode library is first used).
  :config
  ;; kdl-ts-mode itself only calls `treesit-ready-p' (which just warns
  ;; when the grammar is missing).  Ask to install it like the built-in
  ;; -ts-modes do, so opening a .kdl file without the grammar prompts
  ;; instead of silently degrading.
  (defun aiser/kdl-ensure-grammar ()
    (treesit-ensure-installed 'kdl))
  (advice-add #'kdl-ts-mode :before #'aiser/kdl-ensure-grammar))

(provide 'init-kdl)
;;; init-kdl.el ends here
