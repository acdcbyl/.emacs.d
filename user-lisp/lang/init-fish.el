;;; init-fish.el --- fish configuration -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(use-package fish-mode
  :ensure t
  :custom
  (fish-enable-auto-indent t)
  :mode ("\\.fish\\'")
  )

(provide 'init-fish)
;;; init-fish.el ends here.
