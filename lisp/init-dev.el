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
          (conf-toml-mode . toml-ts-mode)
          (go-mode . go-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
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

(use-package
  flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-temp-prefix ".flycheck")
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'right-fringe))

(use-package
  flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config (global-flycheck-eglot-mode 1))

;; flycheck for rust
(use-package
  flycheck-rust
  :ensure t
  :config
  (with-eval-after-load 'rust-ts-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; Set up code folding
(use-package
  treesit-fold
  :ensure t
  :defer 1
  :config (global-treesit-fold-mode))

;; Set up code format
(use-package
  apheleia
  :ensure t
  :diminish apheleia-mode
  :config (apheleia-global-mode +1)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff)))

;; Indentation guide-bars
(use-package
  indent-bars
  :ensure t
  :defer t
  :hook
  ((python-ts-mode json-ts-mode yaml-ts-mode toml-ts-mode rust-ts-mode go-ts-mode) . indent-bars-mode)
  :custom
  (indent-bars-no-descend-lists 'skip)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module")))
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

(use-package yaml-mode :ensure t :mode "\\.ya?ml\\'")

(use-package json-mode :ensure t)

(use-package go-mode :ensure t :defer t)
;; :config
;; (add-hook
;;  'go-ts-mode-hook
;;  (lambda ()
;;    (add-hook 'before-save-hook 'eglot-format-buffer nil t))))

(use-package
  rust-mode
  :ensure t
  :defer t
  :init (setq rust-mode-treesitter-derive t)
  ;; :config
  ;; (add-hook
  ;;  'rust-ts-mode-hook
  ;;  (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  :custom
  (rust-indent-where-clause t)
  (rust-load-optional-libraries t))

(use-package js2-mode :ensure t)
;; :config
;; (add-hook
;;  'js-ts-mode-hook
;;  (lambda ()
;;    (add-hook 'before-save-hook 'eglot-format-buffer nil t))))

(use-package qml-ts-mode
  :load-path "~/.emacs.d/otherlisp/qml-ts-mode/"
  :mode "\\.qml\\'"
  :defer t)
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
  :hook
  ((rust-ts-mode) . eglot-ensure)
  ((go-ts-mode) . eglot-ensure)
  ((python-ts-mode) . eglot-ensure)
  ((qml-ts-mode) . eglot-ensure)

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t) ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore) ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  (add-to-list
   'eglot-server-programs
   '(rust-ts-mode . ("rust-analyzer"))) ; 注意：rust-analyzer 通常不需要 --lsp 参数

  (add-to-list 'eglot-server-programs '(qml-ts-mode . ("qmlls6" "-E")))

  (add-to-list
   'eglot-server-programs '(python-ts-mode . ("ty" "server")))

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

;;eglot doc mouse
(use-package
  eldoc-mouse
  :ensure t
  :defer t
  ;; replace <f1> <f1> to a key you like, "C-h ." maybe. Displaying document on a popup when you press a key.
  :bind
  (:map
   eldoc-mouse-mode-map
   ("<f1> <f1>" . eldoc-mouse-pop-doc-at-cursor)) ;; optional
  :hook eldoc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Project Config
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package
;;  projectile
;;  :ensure t
;;  :hook (after-init . projectile-mode)
;;  :bind (("C-c p" . projectile-command-map))
;;  :config
;;  (setq projectile-mode-line "Projectile")
;;  (setq projectile-track-known-projects-automatically t))

(use-package
  project
  :ensure nil
  :bind (("C-c p" . project-switch-project)))

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
