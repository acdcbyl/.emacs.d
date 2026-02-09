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
        '((bash-mode . bash-ts-mode)
          (conf-toml-mode . toml-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (css-mode . css-ts-mode)))
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
  (flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'right-fringe))

(use-package
  flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config (global-flycheck-eglot-mode 1))

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
  :hook (prog-mode . apheleia-mode))

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
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helpful resources:
;;
;;  - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

(use-package
  eglot
  ;; no :ensure t here because it's built-in

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t) ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore) ; massive perf boost---don't log every event
  )

;;eglot doc mouse
(use-package eldoc-box
  :ensure t
  :custom
  (eldoc-box-lighter nil)
  (eldoc-box-only-multi-line t)
  (eldoc-box-clear-with-C-g t)
  :custom-face
  (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
  (eldoc-box-body ((t (:inherit tooltip))))
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
  :config
  ;; Prettify `eldoc-box' frame
  (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
        (alist-get 'right-fringe eldoc-box-frame-parameters) 8))

;;eglot booster
(use-package
  eglot-booster
  :ensure t
  :vc (:url "https://github.com/jdtsmith/eglot-booster"
            :rev :newest)
  :after eglot
  :init
  (setq eglot-booster-io-only t)
  :config (eglot-booster-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Project Config
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
  project
  :ensure nil
  :after no-littering
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Language Configs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'init-go)
(require 'init-rust)
(require 'init-python)
(require 'init-js)
(require 'init-markdown)
(require 'init-yaml)
(require 'init-json)
(require 'init-qml)
(require 'init-elisp)

(provide 'init-dev)
;;; init-dev.el ends here
