;;; init-evil.el --- Bring vim back -*- lexical-binding: t -*-

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
  (customize-set-variable 'evil-undo-system 'undo-fu)
  (customize-set-variable 'evil-want-C-u-scroll t) ;; Page scroll function
  (customize-set-variable 'evil-want-C-u-delete t) ;; Allow using Ctrl-u to delete to the beginning of the line
  (customize-set-variable 'evil-want-C-g-bindings t)
  (customize-set-variable 'evil-want-minibuffer t)
  :init (setq evil-respect-visual-line-mode t)
  ;; (setq evil-undo-system 'undo-fu)
  ;; Enable this if you want C-u to scroll up, more like pure Vim
                                        ;(setq evil-want-C-u-scroll t)
  :config (evil-mode)
  ;; Configuring initial major mode for some modes
  ;; (evil-set-initial-state 'vterm-mode 'emacs)
  ;; (evil-set-initial-state 'emms-browser-mode 'normal)
  ;; (evil-set-initial-state 'emms-playlist-mode 'normal)
  )
;; evil-collection configuration
(use-package
  evil-collection
  :after evil
  :ensure t
  ;; :custom (evil-collection-corfu-key-themes '(default tab-n-go))
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
(use-package
  general
  :ensure t
  :after evil
  :config
  (require 'nerd-icons)
  ;; For quick esc
  (general-evil-setup)
  (general-imap
    "j" ; imap = insert mode keymap
    (general-key-dispatch
        'self-insert-command
      :timeout 0.20 "k" 'evil-normal-state))
  ;; Define global leader (SPC)
  (general-create-definer
    my-leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  ;; Define localleader (SPC m)
  (general-create-definer
    my-localleader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC l")

  ;; Global leader key binding
  (eval
   `(my-leader-def
      ;; SPC, quit minibuffer.
      "SPC"
      (list 'keyboard-escape-quit :wk ,(format "%s escape" (nerd-icons-mdicon "nf-md-keyboard_esc")))
      ;; Clear highlights
      "S-SPC"
      (list 'lazy-highlight-cleanup :wk ,(format "%s highlight-cleanup" (nerd-icons-mdicon "nf-md-format_clear")))
      ;; Resume
      "'"
      (list 'vertico-repeat :wk ,(format "%s vertico-repeat" (nerd-icons-mdicon "nf-md-replay")))
      ";"
      (list 'avy-resume :wk ,(format "%s avy-resume" (nerd-icons-codicon "nf-cod-debug_continue")))
      "e"
      (list 'dirvish-side :wk ,(format "%s dirvish-side" (nerd-icons-octicon "nf-oct-sidebar_collapse")))
      "l"
      (list :wk ,(format "%s localleader" (nerd-icons-mdicon "nf-md-hammer_wrench")))

      ;;unimpaired style keybinds
      "["
      (list :wk ,(format "%s left prefix" (nerd-icons-mdicon "nf-md-chevron_left")))
      "[e"
      'previous-error
      "]"
      (list :wk ,(format "%s right prefix" (nerd-icons-mdicon "nf-md-chevron_right")))
      "]e"
      'next-error
      ;;helpful
      "h"
      (list :wk ,(format "%s help" (nerd-icons-mdicon "nf-md-help")))
      "hc"
      'helpful-command
      "hC"
      'helpful-callable
      "hv"
      'helpful-variable
      "hk"
      'helpful-key
      "hs"
      'helpful-symbol
      ;; file
      "f"
      (list :wk ,(format "%s files" (nerd-icons-mdicon "nf-md-file")))
      "ff"
      'find-file
      "fF"
      'find-file-other-window
      "f/"
      'find-file-other-window
      "fC"
      '+copy-current-file
      "fD"
      '+delete-current-file
      "fy"
      '+copy-current-filename
      "fR"
      '+rename-current-file
      "fn"
      'make-empty-file
      "fd"
      'make-directory
      "fr"
      'rename-file
      "fl"
      'find-file-literally
      "fj"
      'dired-jump
      "fJ"
      'dired-jump-other-window
      ;; buffer & bookmark
      "b"
      (list :wk ,(format "%s bufmark" (nerd-icons-mdicon "nf-md-dock_window")))
      "bb"
      'switch-to-buffer
      "bB"
      'switch-to-buffer-other-window
      "bc"
      'clone-indirect-buffer
      "bC"
      'clone-indirect-buffer-other-window
      "by"
      '+copy-current-buffer-name
      "bv"
      'revert-buffer-quick
      "bx"
      'scratch-buffer
      "bz"
      'bury-buffer
      ;; --------------
      "bm"
      'bookmark-set
      "bM"
      'bookmark-set-no-overwrite
      "bi"
      'bookmark-insert
      "br"
      'bookmark-rename
      "bd"
      'bookmark-delete
      "bw"
      'bookmark-write
      "bj"
      'bookmark-jump
      "bJ"
      'bookmark-jump-other-window
      "bl"
      'bookmark-bmenu-list
      "bs"
      'bookmark-save
      ;; code
      "c"
      (list :wk ,(format "%s code" (nerd-icons-mdicon "nf-md-code_tags")))
      "cd"
      'rmsbolt-compile
      "cc"
      'compile
      "cC"
      'recompile
      "ck"
      'kill-compilation
      "cl"
      '+switch-to-compilation
      "cw"
      'delete-trailing-whitespace
      "cx"
      'quickrun
      ;; window
      "w"
      (list 'evil-window-map :wk ,(format "%s window-map" (nerd-icons-faicon "nf-fa-window_maximize")))
      "wx"
      'kill-buffer-and-window
      "wu"
      '+transient-tab-bar-history
      "w-"
      'split-window-vertically
      "w/"
      'split-window-horizontally
      ;;Workspace
      "W"
      (list 'evil-window-map :wk ,(format "%s workspaces" (nerd-icons-mdicon "nf-md-view_grid")))
      "Wr"
      'tabspaces-restore-session
      "Ws"
      'tabspaces-save-session
      ;; tab
      "t"
      (list :wk ,(format "%s tab" (nerd-icons-mdicon "nf-md-tab")))
      "tc"
      'centaur-tabs--kill-this-buffer-dont-ask
      "tC"
      'tab-bar-close-tab
      "tg"
      'centaur-tabs-switch-group
      "ti"
      'tab-switcher
      "tn"
      'tab-new
      "to"
      'centaur-tabs-kill-other-buffers-in-current-group
      "tt"
      'tab-bar-switch-to-tab
      "t'"
      'tab-bar-switch-to-recent-tab
      "tr"
      'tab-bar-rename-tab
      "tw"
      'tabspaces-open-or-create-project-and-workspace
      ;; search
      "s"
      (list :wk ,(format "%s search" (nerd-icons-faicon "nf-fa-search")))
      "sj"
      'evil-show-jumps
      "sm"
      'evil-show-marks
      "sr"
      'evil-show-registers
      "si"
      'imenu
      "sp"
      'consult-ripgrep
      "ss"
      'consult-line
      ;; project
      "p"
      (list :wk ,(format "%s project" (nerd-icons-codicon "nf-cod-project")))
      "pf"
      'project-find-file
      "pp"
      'project-switch-project
      "pb"
      'project-switch-to-buffer
      "pg"
      'project-find-regexp
      "pd"
      'project-dired
      "pD"
      'project-find-dir
      "pk"
      'project-kill-buffers
      "ps"
      'project-shell
      "pc"
      'project-compile
      ;; app
      "a"
      (list :wk ,(format "%s app" (nerd-icons-mdicon "nf-md-apps")))
      "aa"
      'org-agenda
      "ac"
      'calendar
      "ag"
      'gnus
      "ai"
      'rcirc
      ;; emms
      "m"
      (list :wk ,(format "%s music" (nerd-icons-mdicon "nf-md-music_note")))
      "mb"
      'mpdel-browser-open
      "ml"
      'mpdel-playlist-open
      "ms"
      'mpdel-song-open
      "mp"
      'libmpdel-playback-play-pause
      "m]"
      'libmpdel-playback-next
      "m["
      'libmpdel-playback-previous
      "mS"
      'libmpdel-stop
      "m+"
      'libmpdel-volume-increase
      "m-"
      'libmpdel-volume-decrease
      "mr"
      'libmpdel-playback-set-random
      "mR"
      'libmpdel-playback-set-repeat
      "mc"
      'libmpdel-playlist-clear
      ;; feeds
      "r" ;;means rss
      (list :wk ,(format "%s feed" (nerd-icons-faicon "nf-fa-rss")))
      "re"
      'elfeed
      "ru"
      'elfeed-update
      ;; Git
      "g"
      (list :wk ,(format "%s magit" (nerd-icons-devicon "nf-dev-git")))
      "gm"
      'magit
      "gp"
      'magit-push
      "gP"
      'magit-pull
      "gb"
      'magit-branch
      "gc"
      'magit-clone
      "gC"
      'magit-clean
      "gl"
      'magit-log
      "gd"
      'magit-dispatch
      "gD"
      'magit-diff
      ;; open
      "o"
      (list :wk ,(format "%s open" (nerd-icons-codicon "nf-cod-link_external")))
      "oc"
      'org-capture
      "ol"
      'org-store-link
      "od"
      'dirvish
      "os"
      'eat-other-window))
  (general-def
    :states '(operator pending visual)
    :keymaps 'override
    "a"  '(:ignore t :wk "arg")
    "a"  #'evil-inner-arg
    "A"  #'evil-outer-arg )
  (eval
   `(my-leader-def
      "z"  (list :ignore t :wk ,(format "%s folding/narrow" (nerd-icons-mdicon "nf-md-unfold_less_horizontal")))
      "zx" #'kill-current-buffer))
  ;; Org mode localleader key binding
  (my-localleader-def
    :major-modes: 'org
    :keymaps
    'org-mode-map
    "."
    'org-goto
    "a"
    'org-archive-subtree
    "d"
    'org-deadline
    "e"
    'org-set-effort
    "f"
    'org-footnote-action
    "l"
    'org-lint
    "o"
    'org-toggle-ordered-property
    "p"
    'org-set-property
    "q"
    'org-set-tags-command
    "r"
    'org-refile
    "s"
    'org-schedule
    "t"
    'org-todo
    "T"
    'org-todo-list
    ;; babel
    "bp"
    'org-babel-previous-src-block
    "bn"
    'org-babel-next-src-block
    "be"
    'org-babel-expand-src-block
    "bg"
    'org-babel-goto-named-src-block
    "bs"
    'org-babel-execute-subtree
    "bb"
    'org-babel-execute-buffer
    "bt"
    'org-babel-tangle
    "bf"
    'org-babel-tangle-file
    "bc"
    'org-babel-check-src-block
    "bi"
    'org-babel-insert-header-arg
    "bI"
    'org-babel-view-src-block-info
    "bk"
    'org-babel-remove-result-one-or-many
    ;; clock
    "cc"
    'org-clock-in
    "cC"
    'org-clock-out
    "cd"
    'org-clock-mark-default-task
    "ce"
    'org-clock-modify-effort-estimate
    "cg"
    'org-clock-goto
    "cl"
    'org-clock-in-last
    "cr"
    'org-clock-report
    "cs"
    'org-clock-display
    "cx"
    'org-clock-cancel
    "c="
    'org-clock-timestamps-up
    "c-"
    'org-clock-timestamps-down
    ;; insert
    "Id"
    'org-insert-drawer
    "In"
    'org-add-note
    "It"
    'org-time-stamp-inactive
    "Ii"
    'org-toggle-inline-images
    "IT"
    'org-time-stamp)

  ;; Emacs Lisp mode localleader key binding
  (my-localleader-def
    :major-modes '(emacs-lisp-mode lisp-interaction-mode)
    "i"   'info-lookup-symbol
    "eb"  'eval-buffer
    "ed"  'eval-defun
    "ee"  'eval-last-sexp
    "el"  'load-library
    "gf"  'find-function
    "gv"  'find-variable
    "gl"  'find-library))

(provide 'init-evil)
;;; init-evil.el ends here
