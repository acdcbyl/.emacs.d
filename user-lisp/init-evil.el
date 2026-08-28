;;; init-evil.el --- Bring vim back -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:
;;

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Core Packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil: vi emulation
(use-package
  evil
  :ensure t
  :preface
  (customize-set-variable 'evil-want-keybinding nil) ;; Use with evil-collection
  (customize-set-variable 'evil-want-integration t) ;; Use with evil-collection
  ;; Emacs 28+ built-in undo/redo; undo history persistence is handled by
  ;; `undo-fu-session' (see init-utils.el), which works with native undo data.
  (customize-set-variable 'evil-undo-system 'undo-redo)
  (customize-set-variable 'evil-want-C-u-scroll t) ;; Page scroll function
  (customize-set-variable 'evil-want-C-u-delete t) ;; Allow using Ctrl-u to delete to the beginning of the line
  (customize-set-variable 'evil-want-C-g-bindings t)
  (customize-set-variable 'evil-want-minibuffer t)
  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-echo-state nil)
  ;; Bug as of 2026-01-12; see https://github.com/emacs-evil/evil/issues/1983
  (defvar evil-mode-buffers '())

  :config (evil-mode)
  ;; If you use Magit, start editing in insert state
  (add-hook 'git-commit-setup-hook 'evil-insert-state)

  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'minibuffer-mode 'insert)
  (evil-set-initial-state 'minibuffer-inactive-mode 'insert)
  )
;; evil-collection configuration
(use-package
  evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-collection-mode-list
        (delq 'corfu evil-collection-mode-list))
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-init))
;; Add comment shortcut
(use-package
  evil-nerd-commenter
  :ensure t
  :after (evil general)
  :config
  (general-def
    :states '(normal visual) ",/" 'evilnc-comment-or-uncomment-lines)
  (general-def
    :states 'normal "gcc" 'evilnc-comment-or-uncomment-lines)
  (general-def
    :states 'visual "gc" 'evilnc-comment-or-uncomment-lines))
;; Modify parentheses in pairs
(use-package
  evil-surround
  :ensure t
  :hook (after-init . global-evil-surround-mode))
;; Convenient parameter modification
(use-package
  evil-args
  :after evil
  :ensure t
  :hook
  ((prog-mode . +config/evil-args-setup)
   (emacs-lisp-mode . +config/evil-args-lisp))
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  (defun +config/evil-args-setup ()
    "Set up evil-args keybindings in current buffer."
    (evil-local-set-key 'normal "L" 'evil-forward-arg)
    (evil-local-set-key 'normal "H" 'evil-backward-arg)
    (evil-local-set-key 'motion "L" 'evil-forward-arg)
    (evil-local-set-key 'motion "H" 'evil-backward-arg)
    (evil-local-set-key 'normal "K" 'evil-jump-out-args))

  (defun +config/evil-args-lisp ()
    (setq-local evil-args-delimiters '(" "))))

;; Use general.el to define leader keys
(use-package general
  :ensure t
  :after (evil nerd-icons)
  :config
  (require 'nerd-icons)
  (general-evil-setup)

  (general-imap
    "j"
    (general-key-dispatch
        'self-insert-command
      :timeout 0.20
      "k" 'evil-normal-state))

  (general-create-definer aiser/leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (general-create-definer aiser/localleader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC l")

  (aiser/leader-def
    "SPC" (list 'keyboard-escape-quit
                :wk (format "%s escape" (nerd-icons-mdicon "nf-md-keyboard_esc")))
    "S-SPC" (list 'lazy-highlight-cleanup
                  :wk (format "%s highlight-cleanup" (nerd-icons-mdicon "nf-md-format_clear")))
    "'"   (list 'vertico-repeat
                :wk (format "%s vertico-repeat" (nerd-icons-mdicon "nf-md-replay")))
    ";"   (list 'avy-resume
                :wk (format "%s avy-resume" (nerd-icons-mdicon "nf-md-play")))
    "l"   (list :wk (format "%s localleader" (nerd-icons-mdicon "nf-md-hammer_wrench")))

    ;; unimpaired style
    "["   (list :wk (format "%s left prefix" (nerd-icons-mdicon "nf-md-chevron_left")))
    "[e"  'previous-error
    "]"   (list :wk (format "%s right prefix" (nerd-icons-mdicon "nf-md-chevron_right")))
    "]e"  'next-error

    ;; help
    "h"   (list :wk (format "%s help" (nerd-icons-mdicon "nf-md-help")))
    "hc"  'helpful-command
    "hC"  'helpful-callable
    "hv"  'helpful-variable
    "hk"  'helpful-key
    "hs"  'helpful-symbol

    ;; file
    "f"   (list :wk (format "%s files" (nerd-icons-mdicon "nf-md-file_document_outline")))
    "ff"  'find-file
    "fF"  'find-file-other-window
    "f/"  'find-file-other-window
    "fC"  '+copy-current-file
    "fD"  '+delete-current-file
    "fy"  '+copy-current-filename
    "fR"  '+rename-current-file
    "fn"  'make-empty-file
    "fd"  'make-directory
    "fr"  'rename-file
    "fl"  'find-file-literally
    "fj"  'dired-jump
    "fJ"  'dired-jump-other-window

    ;; buffer & bookmark
    "b"   (list :wk (format "%s bufmark & buffer" (nerd-icons-mdicon "nf-md-dock_window")))
    "bb"  'switch-to-buffer
    "bB"  'switch-to-buffer-other-window
    "bc"  'clone-indirect-buffer
    "bC"  'clone-indirect-buffer-other-window
    "by"  '+copy-current-buffer-name
    "bv"  'revert-buffer-quick
    "bx"  'scratch-buffer
    "bk"  'kill-current-buffer
    "bz"  'bury-buffer
    ;; bookmark
    "bm"  'bookmark-set
    "bM"  'bookmark-set-no-overwrite
    "bi"  'bookmark-insert
    "br"  'bookmark-rename
    "bd"  'bookmark-delete
    "bw"  'bookmark-write
    "bj"  'bookmark-jump
    "bJ"  'bookmark-jump-other-window
    "bl"  'bookmark-bmenu-list
    "bs"  'bookmark-save

    ;; code
    "c"   (list :wk (format "%s code" (nerd-icons-mdicon "nf-md-code_tags")))
    "cc"  'compile
    "cC"  'recompile
    "ck"  'kill-compilation
    "cl"  '+switch-to-compilation
    "cw"  'delete-trailing-whitespace
    "cx"  'quickrun

    ;; window
    "w"   (list 'evil-window-map
                :wk (format "%s window-map" (nerd-icons-mdicon "nf-md-window_maximize")))
    "ww"  (list 'ace-window
                :wk (format "%s ace-window" (nerd-icons-mdicon "nf-md-window_restore")))
    "wd"  (list 'ace-delete-window
                :wk (format "%s ace-delete-window" (nerd-icons-mdicon "nf-md-window_close")))
    "wx"  'kill-buffer-and-window
    "wu"  '+transient-tab-bar-history
    "w-"  'split-window-vertically
    "w/"  'split-window-horizontally

    ;; workspace
    "W"   (list 'workspace-menu
                :wk (format "%s workspaces" (nerd-icons-mdicon "nf-md-view_grid")))

    ;; tab (workspaces)
    "t"   (list :wk (format "%s tab" (nerd-icons-mdicon "nf-md-tab")))

    ;; native tab-bar (workspaces)
    "tC"  '(tab-bar-close-tab :wk "Close workspace")
    "tN"  '(tab-new :wk "New workspace")
    "tR"  '(tab-bar-rename-tab :wk "Rename workspace")
    "tS"  '(tab-bar-switch-to-tab :wk "Switch workspace")
    "t'"  '(tab-bar-switch-to-recent-tab :wk "Recent workspace")
    "ti"  '(tab-switcher :wk "Workspace switcher")


    ;; search
    "s"   (list :wk (format "%s search" (nerd-icons-mdicon "nf-md-magnify")))
    "sj"  'evil-show-jumps
    "sm"  'evil-show-marks
    "sr"  'evil-show-registers
    "si"  'imenu
    "sp"  'consult-ripgrep
    "ss"  'consult-line

    ;; app
    "a"   (list :wk (format "%s app" (nerd-icons-mdicon "nf-md-apps")))
    "ac"  'calendar
    "aG"  'gnus
    "aa"  'org-agenda
    "at"  'telega
    "ag"  'ghostel
    "ai"  'rcirc

    ;; git
    "g"   (list :wk (format "%s git/magit" (nerd-icons-mdicon "nf-md-git")))
    "gm"  'magit-status
    "gb"  'magit-blame
    "gp"  'magit-push
    "gP"  'magit-pull
    "gB"  'magit-branch
    "gc"  'magit-clone
    "gC"  'magit-clean
    "gl"  'magit-log
    "gd"  'magit-dispatch
    "gD"  'magit-diff
    "gt"  'git-timemachine
    "gn"  'diff-hl-next-hunk
    "gN"  'diff-hl-previous-hunk
    "gs"  'diff-hl-stage-hunk
    "gr"  'diff-hl-revert-hunk

    ;; open
    "o"   (list :wk (format "%s open" (nerd-icons-mdicon "nf-md-open_in_new")))
    "oc"  'org-capture
    "ol"  'org-store-link
    "od"  'dired
    "ow"  'workspace-menu
    "oi"  'ibuffer
    "os"  'symbols-outline-show

    ;; folding
    "z" (list :wk (format "%s folding/narrow" (nerd-icons-mdicon "nf-md-unfold_less_horizontal")))
    "zc" 'hs-hide-block
    "zC" 'hs-hide-all
    "zo" 'hs-show-block
    "zO" 'hs-show-all
    "zt" 'hs-toggle-hiding
    )

  (aiser/localleader-def
    :major-modes '(emacs-lisp-mode lisp-interaction-mode)
    "i"  'info-lookup-symbol
    "eb" 'eval-buffer
    "ed" 'eval-defun
    "ee" 'eval-last-sexp
    "el" 'load-library
    "gf" 'find-function
    "gv" 'find-variable
    "gl" 'find-library)

  ;; tab-bar navigation
  (general-def
    :states 'normal
    "gt"  'tab-bar-switch-to-next-tab
    "gT"  'tab-bar-switch-to-prev-tab
    "]t"  'tab-bar-switch-to-next-tab
    "[t"  'tab-bar-switch-to-prev-tab)
  )

;; Deferred until `general''s :config has created `aiser/leader-def'
;; (which happens when nerd-icons loads).
(with-eval-after-load 'nerd-icons
  ;; Emacs 31: built-in CJK fullwidth <-> halfwidth conversion
  ;; (defined in text-mode but preloaded).  With no active region they
  ;; act on the word at point; with a region, on the whole region.
  (aiser/leader-def
    "xf" 'fullwidth-region
    "xh" 'halfwidth-region)

  ;; Emacs 31: whole-layout transforms inside the window map (SPC w).
  ;; NOTE evil's own r/R rotate window *buffers* cyclically; these new
  ;; commands transform the split *layout tree* itself:
  ;;   t   transpose   - every horizontal split becomes vertical & vice versa
  ;;   y   rotate clockwise / Y anticlockwise
  ;;   F   flip left-right / V flip top-down
  (general-def :keymaps 'evil-window-map
    "t" 'window-layout-transpose
    "y" 'window-layout-rotate-clockwise
    "Y" 'window-layout-rotate-anticlockwise
    "F" 'window-layout-flip-leftright
    "V" 'window-layout-flip-topdown)

  ;; Free C-h inside the window map: it shadows the generic help-char
  ;; handler that which-key uses for paging while the SPC w popup is up.
  ;; Window navigation is unaffected -- lowercase h/j/k/l in the same map
  ;; do exactly what C-h/C-j/C-k/C-l did.
  (general-def :keymaps 'evil-window-map "C-h" nil))

(provide 'init-evil)
;;; init-evil.el ends here
