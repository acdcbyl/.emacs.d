;;; init-treemacs.el --- A tree layout file explorer -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(use-package treemacs
  :ensure t
  :functions (treemacs-follow-mode
              treemacs-filewatch-mode
              treemacs-git-mode
              treemacs-set-scope-type)
  :custom-face
  (cfrs-border-color ((t (:inherit posframe-border))))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        treemacs-user-mode-line-format   'none
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-width                   30
        )

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (use-package
    treemacs-nerd-icons
    :ensure t
    :autoload treemacs-nerd-icons-config
    :init (treemacs-nerd-icons-config))

  (use-package
    treemacs-magit
    :ensure t
    :demand t)

  (use-package
    treemacs-tab-bar
    :ensure t
    :demand t
    :config (treemacs-set-scope-type 'Tabs))

  (use-package
    treemacs-projectile
    :ensure t
    :after projectile)

  (use-package treemacs-evil
    :ensure t
    :after  evil
    :ensure t)

  (use-package treemacs-persp
    :ensure t
    :after persp-mode
    :ensure t
    :config (treemacs-set-scope-type 'Perspectives))
  )

(provide 'init-treemacs)
;;; init-treemacs.el ends here.
