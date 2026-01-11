;;; init-feed.el --- add feed reader for emacs -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package elfeed :ensure t :defer t)

(use-package
 elfeed-org
 :ensure t
 :after elfeed
 :config
 (elfeed-org)
(setq rmh-elfeed-org-files (list "~/.emacs.d/feeds.org")))

(provide 'init-feed)
;;; init-feed.el ends here
