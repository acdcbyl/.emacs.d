;;; init-dev.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package eat :ensure t :defer t)

(use-package quickrun :ensure t :defer t)

(use-package
 emacs
 :config
 ;; Treesitter config

 ;; Tell Emacs to prefer the treesitter mode
 ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
 (setq major-mode-remap-alist
       '((yaml-mode . yaml-ts-mode)
         (bash-mode . bash-ts-mode)
         (go-mode . go-ts-mode)
         (rust-mode . rust-ts-mode)
         (js2-mode . js-ts-mode)
         (typescript-mode . typescript-ts-mode)
         (json-mode . json-ts-mode)
         (css-mode . css-ts-mode)
         (python-mode . python-ts-mode)))
 :hook
 ;; Auto parenthesis matching
 ((prog-mode . electric-pair-mode)))

;; Use flycheck for checking

(use-package flycheck :ensure t :init (global-flycheck-mode))

(use-package
 flycheck-eglot
 :ensure t
 :after (flycheck eglot)
 :config (global-flycheck-eglot-mode 1))

;; Set up code folding
(use-package
 treesit-fold
 :ensure t
 :config (global-treesit-fold-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit :ensure t :bind (("C-x g" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode :ensure t)

(use-package json-mode :ensure t)

(use-package
 go-mode
 :ensure t
 ;; :mode ("\\.go\\'" . go-mode)
 :config
 (add-hook
  'go-ts-mode-hook
  (lambda ()
    (add-hook 'before-save-hook 'eglot-format-buffer nil t))))

(use-package
 rust-mode
 :ensure t
 :config
 (add-hook
  'rust-ts-mode-hook
  (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
 :custom
 (rust-indent-where-clause t)
 (rust-load-optional-libraries t))

(use-package qml-mode :ensure t :mode ("\\.qml\\'" . qml-mode))
;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helpful resources:
;;
;;  - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

(use-package
 eglot
 ;; no :ensure t here because it's built-in

 ;; Configure hooks to automatically turn-on eglot for selected modes
 :hook ((rust-ts-mode) . eglot-ensure) ((go-ts-mode) . eglot-ensure)

 :custom (eglot-send-changes-idle-time 0.1)
 (eglot-extend-to-xref t) ; activate Eglot in referenced non-project files

 :config
 (fset #'jsonrpc--log-event #'ignore) ; massive perf boost---don't log every event
 ;; Sometimes you need to tell Eglot where to find the language server
 (add-to-list
  'eglot-server-programs
  '(rust-ts-mode . ("rust-analyzer"))) ; 注意：rust-analyzer 通常不需要 --lsp 参数

 (add-to-list 'eglot-server-programs '(qml-mode . ("qmlls6" "-E")))

 (add-to-list
  'eglot-server-programs
  '(go-ts-mode
    .
    ("gopls"
     :initializationOptions
     (:hints
      (:assignVariableTypes
       t
       :compositeLiteralFields t
       :parameterNames t
       :functionTypeParameters t))))))

;;eglot doc box
(use-package
 eldoc-box
 :ensure t
 :config (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
 (add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors
           0 t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Project Config
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 projectile
 :ensure t
 :hook (after-init . projectile-mode)
 :bind (("C-c p" . projectile-command-map))
 :config
 (setq projectile-mode-line "Projectile")
 (setq projectile-track-known-projects-automatically t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Snippet Config
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; yasnippet
(use-package
 yasnippet
 :ensure t
 :hook
 ((prog-mode . yas-minor-mode) (text-mode . yas-minor-mode))
 :config (yas-reload-all))

;; Official snippet collection
(use-package yasnippet-snippets :ensure t :after yasnippet)

(provide 'init-dev)
;;; init-dev.el ends here
