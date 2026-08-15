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

;; Switch to the *compilation* buffer (bound in init-evil.el: SPC c l)
(defun +switch-to-compilation ()
  "Switch to the *compilation* buffer in another window, if it exists."
  (interactive)
  (if (get-buffer "*compilation*")
      (switch-to-buffer-other-window "*compilation*")
    (user-error "No *compilation* buffer; run compile first")))

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
            (qmljs . ("https://github.com/yuja/tree-sitter-qmljs" "master" "src"))
            (yaml . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml"))))
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
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.2)
  (indent-bars-pad-frac 0.1)
  (indent-bars-highlight-current-depth
   '(:width 0.45
            :pad 0.025
            :blend 1.0))
  )

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

;; Magit: the best Git client for Emacs
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :custom
  ;; Display magit status in current window while retaining a clean layout
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  ;; Highlight word-level diffs inside the current hunk automatically
  (magit-diff-refine-hunk 'all)
  ;; Save modified file buffers in the repository before executing magit commands
  (magit-save-repository-buffers 'autosave)
  ;; Set maximum summary line length for git commit messages
  (git-commit-summary-max-length 72))

;; Magit-prime: asynchronous cache pre-warming for faster magit status refreshes
(use-package magit-prime
  :ensure t
  :after magit
  :config
  (magit-prime-mode 1))

;; Git modes: syntax highlighting for .gitignore, .gitconfig, and .gitattributes
(use-package git-modes
  :ensure t)

;; Git timemachine: step through historical git revisions of a file
(use-package git-timemachine
  :ensure t
  :defer t)

;; Auto-enable smerge-mode when merge conflict markers are detected
(use-package smerge-mode
  :ensure nil
  :hook (find-file . (lambda ()
                       (save-excursion
                         (goto-char (point-min))
                         (when (re-search-forward "^<<<<<<< " nil t)
                           (smerge-mode 1))))))

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
(require 'init-fish)

(provide 'init-dev)
;;; init-dev.el ends here
