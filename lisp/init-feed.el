;;; init-feed.el --- add feed reader for emacs -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Emacs 31: let SHR leave line breaking to the window (visual-wrap)
;; instead of hard-wrapping at `fill-column'.  Much better for CJK text,
;; which has no spaces to re-fill from.  Affects eww and elfeed articles.
(setopt shr-fill-text nil)
(add-hook 'elfeed-show-mode-hook #'visual-wrap-prefix-mode)

(use-package elfeed
  :ensure t
  :defer t
  :general
  (aiser/leader-def
    "r"   (list :wk (format "%s feed" (nerd-icons-mdicon "nf-md-rss")))
    "re"  'elfeed
    "ru"  'elfeed-update))

(use-package elfeed-org
  :ensure t
  :after elfeed)

;; Lightweight nano-style elfeed UI.  No extra dependencies: it only needs
;; elfeed + nerd-icons, both already installed.
;; (see user-lisp/elfeed-nano.el)
(use-package elfeed-nano
  :demand t
  :config
  (elfeed-nano-mode)
  (setq rmh-elfeed-org-files
        (list (expand-file-name "feeds.org" user-emacs-directory)))
  (elfeed-org)
  ;; lambda-line's elfeed segment calls the OLD function name
  ;; `elfeed-search--count-unread', which current elfeed renamed to
  ;; `elfeed-search--unread-count'.  Provide a compat alias so the
  ;; mode-line doesn't error with (void-function ...).
  (when (fboundp 'elfeed-search--unread-count)
    (defalias 'elfeed-search--count-unread #'elfeed-search--unread-count)))

(provide 'init-feed)
;;; init-feed.el ends here
