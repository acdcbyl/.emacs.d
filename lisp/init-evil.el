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
 :init (setq evil-respect-visual-line-mode t)
 ;; (setq evil-undo-system 'undo-fu)
 ;; Enable this if you want C-u to scroll up, more like pure Vim
 ;(setq evil-want-C-u-scroll t)
 :config (evil-mode)
 ;; Configuring initial major mode for some modes
 (evil-set-initial-state 'vterm-mode 'emacs)
 ;; (evil-set-initial-state 'emms-browser-mode 'normal)
 ;; (evil-set-initial-state 'emms-playlist-mode 'normal)
 )
;; evil-collection configuration
(use-package
 evil-collection
 :after evil
 :ensure t
 :config (evil-collection-init))
;; Add comment shortcut
(use-package
 evil-nerd-commenter
 :ensure t
 :after evil
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
 :hook (emacs-lisp-mode . +config/evil-args-lisp)
 :config
 (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
 (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
 (define-key evil-normal-state-map "L" 'evil-forward-arg)
 (define-key evil-normal-state-map "H" 'evil-backward-arg)
 (define-key evil-motion-state-map "L" 'evil-forward-arg)
 (define-key evil-motion-state-map "H" 'evil-backward-arg)
 (define-key evil-normal-state-map "K" 'evil-jump-out-args)
 (defun +config/evil-args-lisp ()
   (setq-local evil-args-delimiters '(" "))))
;; Quick exit
;; (use-package
;;  evil-escape
;;  :ensure t
;;  :after evil
;;  :init
;;  (setq evil-escape-key-sequence "jk") ;; Set shortcut key sequence
;;  (setq evil-escape-delay 0.2) ;; Set the time between two key presses
;;  :config (evil-escape-mode 1))

;; Use general.el to define leader keys
(use-package
 general
 :ensure t
 :after evil
 :config
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
  :prefix "SPC m")

 ;; Global leader key binding
 (my-leader-def
  ;; SPC, quit minibuffer.
  "SPC"
  'keyboard-escape-quit
  ;; Clear highlights
  "S-SPC"
  'lazy-highlight-cleanup
  ;; Resume
  "'"
  'vertico-repeat
  ";"
  'avy-resume
  ;; file
  "f"
  '(:wk "󰈔 files")
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
  'recentf-open-files
  "fl"
  'find-file-literally
  "fj"
  'dired-jump
  "fJ"
  'dired-jump-other-window
  ;; buffer & bookmark
  "b"
  '(:wk "󰙒 bufmark")
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
  '(:wk "󰈮 code")
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
  'evil-window-map
  "wx"
  'kill-buffer-and-window
  "wu"
  '+transient-tab-bar-history
  "w-"
  'split-window-vertically
  "w/"
  'split-window-horizontally
  ;; tab
  "t"
  '(:wk "󰓩 tab")
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
  ;; search
  "s"
  '(:wk " search")
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
  'projectile-command-map
  ;; app
  "a"
  '(:wk "󰀻 app")
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
  '(:wk "󰝚 music")
  "mb"
  'mpdel-browser-open
  "ml"
  'mpdel-playlist-open
  "ms"
  'mpdel-song-open
  "mp"
  'libmpdel-play
  "mP"
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
  'libmpdel-playback-random
  "mR"
  'libmpdel-playback-repeat
  "mc"
  'libmpdel-current-playlist-clear
  ;; open
  "o"
  '(:wk " open")
  "oc"
  'org-capture
  "ol"
  'org-store-link
  "ot"
  'dirvish-side
  "os"
  'eat-other-window)

 ;; Org mode localleader key binding
 (with-eval-after-load 'org
   (my-localleader-def
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
    "id"
    'org-insert-drawer
    "in"
    'org-add-note
    "it"
    'org-time-stamp-inactive
    "iT"
    'org-time-stamp))

 ;; Emacs Lisp mode localleader key binding
 (with-eval-after-load 'elisp-mode
   (my-localleader-def
    :keymaps
    '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "i"
    'info-lookup-symbol
    ;; eval
    "eb"
    'eval-buffer
    "ed"
    'eval-defun
    "ee"
    'eval-last-sexp
    "el"
    'load-library
    ;; goto
    "gf"
    'find-function
    "gv"
    'find-variable
    "gl"
    'find-library)))

(provide 'init-evil)
;;; init-evil.el ends here
