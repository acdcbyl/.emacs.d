;;; init-dev.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package eat :ensure t :defer t)

(use-package ghostel  :ensure t :defer t)

(use-package quickrun :ensure t :defer t)

(use-package
  emacs
  :config
  ;; Treesitter config

  ;; Emacs 31 can remap built-in modes to tree-sitter modes directly.
  (setopt treesit-enabled-modes t)
  (setopt treesit-auto-install-grammar 'ask)
  ;; Amount to highlight: integer between 1-4; 4 is max highlighting
  (setopt treesit-font-lock-level 3)
  (setopt treesit-extra-load-path
          (list (locate-user-emacs-file "tree-sitter")))
  (setopt treesit-language-source-alist
          '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
            (c . ("https://github.com/tree-sitter/tree-sitter-c"))
            (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
            (css . ("https://github.com/tree-sitter/tree-sitter-css"))
            (go . ("https://github.com/tree-sitter/tree-sitter-go"))
            (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod"))
            (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
            (json . ("https://github.com/tree-sitter/tree-sitter-json"))
            (kdl . ("https://github.com/tree-sitter-grammars/tree-sitter-kdl"))
            (lua . ("https://github.com/tree-sitter-grammars/tree-sitter-lua"))
            (python . ("https://github.com/tree-sitter/tree-sitter-python"))
            (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
            (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
            (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
            (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
            (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
            (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown-inline/src"))
            (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

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

;; Outline
(use-package symbols-outline
  :ensure t
  :init
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local symbols-outline-fetch-fn #'symbols-outline-lsp-fetch)))
  :config
  (setq symbols-outline-window-position 'right)
  (symbols-outline-follow-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit :ensure t :bind (("C-x g" . magit-status)))

;; A Magit extension that primes the magit cache in parallel before refresh, reducing refresh times
(use-package magit-prime
  :ensure t
  :defer t
  :after magit
  :config
  (magit-prime-mode))
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
  :ensure nil
  ;; no :ensure t here because it's built-in

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t) ; activate Eglot in referenced non-project files

  :config
  ;; Avoid changing line heights if your font is wonky. See
  ;; https://github.com/joaotavora/eglot/discussions/1492
  (setopt eglot-code-action-indicator "h")
  (advice-add #'jsonrpc--log-event :override #'ignore) ; massive perf boost---don't log every event
  )

;;eglot doc
(use-package
  eldoc-mouse
  :ensure t
  :defer t
  :custom
  (eldoc-mouse-posframe-border-width 1)
  (eldoc-mouse-posframe-border-color
   (face-attribute 'posframe-border :background nil 'default))
  (eldoc-mouse-posframe-fringe-width 8)
  (eldoc-mouse-posframe-override-parameters
   '((left-fringe         . 8)
     (right-fringe        . 8)
     (internal-border-width . 1)
     (drag-internal-border  . t)))
  :bind
  (:map
   eldoc-mouse-mode-map
   ("<f1> <f1>" . eldoc-mouse-pop-doc-at-cursor)) ;; optional
  :hook (eglot-managed-mode emacs-lisp-mode))


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

;; Yasnippet Completion At Point Function
(use-package yasnippet-capf
  :ensure t
  :commands yasnippet-capf
  :functions cape-capf-super eglot-completion-at-point
  :hook (((conf-mode prog-mode text-mode) . aiser/yasnippet-capf-h)
         (eglot-managed-mode . aiser/eglot-capf))
  :init
  (defun aiser/yasnippet-capf-h ()
    (add-to-list 'completion-at-point-functions #'yasnippet-capf))

  ;; Making a Cape Super Capf for Eglot
  ;; https://github.com/minad/corfu/wiki#making-a-cape-super-capf-for-eglot
  (defun aiser/eglot-capf ()
    (setq-local completion-at-point-functions
                (list
	         (cape-capf-super
		  #'eglot-completion-at-point
		  #'yasnippet-capf)))))


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
(require 'init-kdl)
(require 'init-lua)
(require 'init-elisp)

(provide 'init-dev)
;;; init-dev.el ends here
