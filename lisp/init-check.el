;; init-check.el --- Initialize check configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Flymake's Elisp byte-compile checker consults `trusted-content';
;; mark the local config as trusted so it is not flagged untrusted.
(setopt trusted-content (list (expand-file-name "lisp/" user-emacs-directory)))

(use-package flymake
  :diminish
  :functions aiser/elisp-flymake-byte-compile
  :bind ("C-c f" . flymake-show-buffer-diagnostics)
  :hook prog-mode
  :custom
  (flymake-no-changes-timeout nil)
  (flymake-suppress-zero-counters t)
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-margin-indicator-position 'right-margin)
  :config
  ;; Check elisp with `load-path'
  (defun aiser/elisp-flymake-byte-compile (fn &rest args)
    "Wrapper for `elisp-flymake-byte-compile'."
    (let ((elisp-flymake-byte-compile-load-path
           (append elisp-flymake-byte-compile-load-path load-path)))
      (apply fn args)))
  (advice-add 'elisp-flymake-byte-compile :around #'aiser/elisp-flymake-byte-compile))

;; Display Flymake errors with overlays
(use-package flyover
  :ensure t
  :diminish
  :custom
  (flyover-checkers '(flymake))
  (flyover-background-lightness 60)
  (flyover-icon-background-tint-percent 50)
  :hook flymake-mode)

(provide 'init-check)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-check.el ends here
