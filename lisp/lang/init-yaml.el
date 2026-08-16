;;; init-yaml.el --- YAML configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; NOTE: the third-party `yaml-mode' package is not needed.  On git
;; Emacs (31+) the built-in yaml-ts-mode handles .ya?ml natively via
;; the startup auto-mode-alist entry `("\\.ya?ml\\'" .
;; yaml-ts-mode-maybe)'.  The explicit :mode below just documents the
;; mapping (a direct entry, equivalent to the -maybe one when the
;; grammar is installed).
;;
;; yaml-ts-mode derives from text-mode, so the `prog-mode' apheleia
;; hook does not cover it; apheleia-mode is enabled for yaml-ts-mode
;; via the apheleia use-package in init-dev.el (prettier-yaml via
;; `apheleia-npx', needs prettier in the project's node_modules or on
;; PATH).

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.ya?ml\\'")

(provide 'init-yaml)
;;; init-yaml.el ends here
