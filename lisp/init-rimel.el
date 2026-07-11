;;; init-rimel.el --- Bring better Chinese input method -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; A emacs dynamic module provide librime bindings for emacs
(use-package
  liberime
  :ensure t
  :defer t
  :init
  (setq liberime-auto-build t))

(use-package
  rimel
  :ensure t
  :defer t
  :init (setq default-input-method "rimel")
  :config
  (setq rimel-schema "luna_pinyin_simp")
  (setq rimel-disable-predicates
        '(rimel-predicate-prog-in-code-p
          rimel-predicate-after-alphabet-char-p
          rimel-predicate-current-uppercase-letter-p
          rimel-predicate-evil-mode-p))
  (add-to-list 'rimel-disable-predicates 'rimel-predicate-org-in-src-block-p)
  )

(provide 'init-rimel)
;;; init-rimel.el ends here
