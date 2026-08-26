;;; init-dev.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  ;; Ask before downloading missing grammars (needs `treesit' loaded;
  ;; setopt is fine on not-yet-defined variables).
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
  ;; Auto parenthesis matching, built-in code folding (hideshow is
  ;; tree-sitter aware since 31)
  ((prog-mode . electric-pair-mode)
   ((prog-mode yaml-ts-mode) . hs-minor-mode)))

;; Set up code format
(use-package
  apheleia
  :ensure t
  :diminish apheleia-mode
  ;; yaml-ts-mode derives from text-mode, so the prog-mode hook does
  ;; not cover it; it needs its own entry (prettier-yaml via
  ;; `apheleia-npx', needs prettier in the project's node_modules or on
  ;; PATH).
  :hook ((prog-mode yaml-ts-mode) . apheleia-mode))

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
                       ;; Skip huge buffers: scanning for conflict
                       ;; markers from point-min is expensive there.
                       (when (< (buffer-size) (* 2 1024 1024))
                         (save-excursion
                           (goto-char (point-min))
                           (when (re-search-forward "^<<<<<<< " nil t)
                             (smerge-mode 1)))))))

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
  (eglot-send-changes-idle-time 0.3)
  (eglot-extend-to-xref t) ; activate Eglot in referenced non-project files
  ;; Don't block Emacs while the LSP server initializes; connect in background.
  (eglot-sync-connect 0)
  ;; Kill the server when its last managed buffer is closed -- useful when
  ;; switching between multiple projects.
  (eglot-autoshutdown t)
  ;; Suppress mode-line progress redisplay churn while servers index/build.
  (eglot-report-progress nil)

  :config
  ;; Avoid changing line heights if your font is wonky. See
  ;; https://github.com/joaotavora/eglot/discussions/1492
  (setopt eglot-code-action-indicator "h")
  ;; Emacs 32 way of taming the jsonrpc events buffer; no need to
  ;; advice `jsonrpc--log-event' any more.
  (setopt eglot-events-buffer-config '(:size 0 :format full))
  ;; Ceiling on file watches: protects fd/memory/startup cost on large
  ;; repos (node_modules etc.).  NOTE: a plain defvar in Emacs 32, so
  ;; this must live in :config (not :custom).
  ;; (setopt eglot-max-file-watches 3000)
  ;; Formatting is delegated to Apheleia; drop the server's formatting
  ;; capabilities so it never advertises/uses them.
  (dolist (cap '(:documentFormattingProvider
                 :documentRangeFormattingProvider
                 :documentOnTypeFormattingProvider))
    (add-to-list 'eglot-ignored-server-capabilities cap))
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
;; Tempel: lightweight Elisp-native templates. Registers as a
;; completion-at-point backend, so templates show up directly in the
;; Corfu popup alongside cape-dabbrev etc.
(use-package tempel
  :ensure t
  ;; Bindings take effect while point is inside an expanded template.
  :bind (:map tempel-map
              ("TAB" . tempel-next)
              ("S-TAB" . tempel-previous)
              ("RET" . tempel-done))
  :init
  ;; Personal templates live here (default); one template per .eld file
  ;; (setopt tempel-path (locate-user-emacs-file "templates"))
  (defun aiser/tempel-setup-capf ()
    "Add Tempel's capf ahead of the other backends."
    (add-to-list 'completion-at-point-functions #'tempel-complete))
  :hook ((conf-mode prog-mode text-mode) . aiser/tempel-setup-capf))

;; Ready-made template collection (replaces yasnippet-snippets).
;; Templates register automatically for all supported major modes.
(use-package tempel-collection
  :ensure t
  :after tempel)

;; Making a Cape Super Capf for Eglot
;; https://github.com/minad/corfu/wiki#making-a-cape-super-capf-for-eglot
(defun aiser/eglot-capf ()
  (setq-local completion-at-point-functions
              (list
               (cape-capf-super
                #'eglot-completion-at-point
                #'tempel-complete)
               #'cape-file)))
(add-hook 'eglot-managed-mode-hook #'aiser/eglot-capf)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Language Configs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Language configs are NOT required eagerly: that would pull
;; go/rust/python/js/... setup into the startup critical path.
;;
;; With `treesit-enabled-modes' set to t, startup already installs
;; `auto-mode-alist' entries like ("\\.go\\'" . go-ts-mode-maybe), so
;; built-in ts-modes are reached without any help here.  For external
;; packages we register their file extensions up-front, then load each
;; full config only when its major-mode library is first loaded
;; (`with-eval-after-load' handlers run while the mode's library is
;; being autoloaded, i.e. before mode hooks run, so eglot-ensure fires
;; for the very first buffer as well).

;; File extensions for third-party modes (built-in ts-modes are covered
;; by `treesit-enabled-modes').
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.kdl\\'" . kdl-ts-mode))
(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))
;; Built-in json-ts-mode only covers \.json\' via the js-json-mode remap;
;; handle .jsonld the same way.
(add-to-list 'auto-mode-alist '("\\.jsonld\\'" . js-json-mode))

(with-eval-after-load 'go-ts-mode          (require 'init-go))
(with-eval-after-load 'rust-ts-mode        (require 'init-rust))
;; NOTE: there is no `python-ts-mode' feature — python's treesit mode
;; lives in the built-in `python' feature (python.el).
(with-eval-after-load 'python              (require 'init-python))
(with-eval-after-load 'js                  (require 'init-js))
(with-eval-after-load 'typescript-ts-mode  (require 'init-js))
(with-eval-after-load 'markdown-ts-mode    (require 'init-markdown))
(with-eval-after-load 'qml-ts-mode         (require 'init-qml))
(with-eval-after-load 'kdl-ts-mode         (require 'init-kdl))
(with-eval-after-load 'lua-ts-mode         (require 'init-lua))
(with-eval-after-load 'fish-mode           (require 'init-fish))

;; Bootstrap for third-party packages that PROVIDE their own major mode
;; (qml-ts-mode, kdl-ts-mode, fish-mode): while such a package is absent,
;; nothing autoloads its library, so the `with-eval-after-load' handlers
;; above never fire and the config file -- which carries the :vc/:ensure
;; install recipe -- stays unreachable.  On first open of a matching file,
;; require the config (installing its package), then re-select the major
;; mode for this buffer.  When everything is already installed, this is
;; a no-op: the eval-after-load handler has already loaded the config.
(defvar aiser/lang-config-bootstraps nil
  "List of (FILE-REGEXP . CONFIG-FEATURE) pairs.
See `aiser/lang-config-bootstrap'.")

(defun aiser/lang-config-bootstrap ()
  "Load missing lang configs on first open of their file type."
  (when (and buffer-file-name aiser/lang-config-bootstraps)
    (let ((file buffer-file-name))
      (dolist (entry aiser/lang-config-bootstraps)
        (when (string-match-p (car entry) file)
          (setq aiser/lang-config-bootstraps
                (delete entry aiser/lang-config-bootstraps))
          (unless (featurep (cdr entry))
            (require (cdr entry))
            ;; The file opened before its major mode was available;
            ;; redo mode selection now that the package is installed.
            (when (eq major-mode 'fundamental-mode)
              (normal-mode))))))))
(add-hook 'find-file-hook #'aiser/lang-config-bootstrap)

(mapc (lambda (entry)
        (add-to-list 'aiser/lang-config-bootstraps entry))
      '(("\\.qml\\'"  . init-qml)
        ("\\.kdl\\'"  . init-kdl)
        ("\\.fish\\'" . init-fish)))

;; Elisp config is always relevant to this setup; load it eagerly.
(require 'init-elisp)

(provide 'init-dev)
;;; init-dev.el ends here
