;;; init-base.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(setopt initial-major-mode 'fundamental-mode) ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil) ; this information is useless for most
;; Show time
(display-time)
;; Don't delete files diretly
(setq delete-by-moving-to-trash t)
;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
(setopt auto-revert-interval 5)
;; Keep nil (the default): checking VC info on every revert shells out
;; to git each time, which stutters on network mounts / huge repos.
;; diff-hl refreshes its own indicators anyway.
(setopt auto-revert-check-vc-info nil)
;; Restrict version control backends to Git only to avoid unnecessary I/O checks
(setq vc-handled-backends '(Git))
(global-auto-revert-mode)

;; undo-limit
(setq undo-limit 67108864) ; 64mb.
(setq undo-strong-limit 100663296) ; 96mb.
(setq undo-outer-limit 1006632960) ; 960mb.

;; Save history of minibuffer
(savehist-mode)

;; Save cursor place
(save-place-mode)

;; Save clipboard before kill, deduplicate kill ring
(setopt save-interprogram-paste-before-kill t)
(setopt kill-do-not-save-duplicates t)
;; Emacs 30+: strip common leading indentation when yanking multi-line
;; regions into different contexts.
(kill-ring-deindent-mode 1)

;; Don't ping URL-looking things in find-file
(setopt ffap-machine-p-known 'reject)

;; Remember projects for project.el
(setq project-remember-projects-on-exit t)

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control) ; You can use other modifiers here

;; Rebalance windows automatically when splitting
(setopt window-combination-resize t)

;; Prefer horizontal split on landscape monitors
(setopt split-window-preferred-direction 'longest)

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; y/n instead of yes/no
(setq use-short-answers t)

;; Makes it easier to repeat commands; e.g. C-x o C-x o becomes C-x o o
(repeat-mode)

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun backup-file-name (fpath)
  "Return a new file path of a given file FPATH.
If the new path's directories does not exist, create them."
  (let*
      ((backupRootDir (expand-file-name "~/.emacs.d/emacs-backup/"))
       (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath)) ; remove Windows driver letter in path
       (backupFilePath
        (replace-regexp-in-string
         "//" "/"
         (concat backupRootDir filePath "~"))))
    (make-directory (file-name-directory backupFilePath) t)
    backupFilePath))
(setopt make-backup-file-name-function 'backup-file-name)

;; bidi settings live in early-init.el (bidi-display-reordering is intentional)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Discovery aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun childframe-workable-p ()
  "Whether childframe is workable."
  (and (not noninteractive)
       (not emacs-basic-display)
       (or (display-graphic-p)
           (featurep 'tty-child-frames))
       (eq (frame-parameter (selected-frame) 'minibuffer) 't)))
;; Show the help buffer after startup
                                        ;(add-hook 'after-init-hook 'help-quick)
(setopt view-lossage-auto-refresh t)

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package
  which-key
  :ensure nil
  :defer 0.5
  :config
  (setq which-key-prefix-prefix ""              ; remove +
        which-key-separator " "
        which-key-sort-order 'which-key-local-then-key-order
        which-key-idle-delay 0.4
        which-key-add-column-padding 1)
  (which-key-mode))

;; Emacs 30 includes EditorConfig support.
(use-package
  editorconfig
  :ensure nil
  :hook (after-init . editorconfig-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Most completion settings are handled by Vertico/Corfu/Orderless in init-completion.el
(setopt enable-recursive-minibuffers t) ; Use the minibuffer whilst in the minibuffer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode line information
(setopt line-number-mode t) ; Show current line in modeline
(setopt column-number-mode t) ; Show column as well
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)           ; Set a minimum width
(setopt x-underline-at-descent-line nil) ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t) ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil) ; By default, don't underline trailing spaces

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)


(setopt indent-tabs-mode nil)
;; (setopt tab-width 4)  ; per-language defaults are set by major modes

;; Misc. UI tweaks
(blink-cursor-mode -1) ; Steady cursor
(pixel-scroll-precision-mode) ; Smooth scrolling

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Emacs 30+ `visual-wrap-prefix-mode' (built-in replacement for
;; adaptive-wrap): hanging indent for wrapped lines in Org/Markdown.
(add-hook 'text-mode-hook #'visual-wrap-prefix-mode)
(add-hook 'org-mode-hook #'visual-wrap-prefix-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; Show matching delimiters
(setopt show-paren-delay 0)
(setopt show-paren-mode t)
(setopt show-paren-style 'parenthesis)
(setopt show-paren-context-when-offscreen 'overlay)

;; Fonts
(defun font-available-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("PragmataPro Mono Liga")
             when (font-available-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height 140))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Material Symbols Rounded" "Symbola" "Apple Symbols" "Segoe UI Symbol"  "Symbol")
             when (font-available-p font)
             return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-available-p font)
             return (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("LXGW WenKai Mono" "WenQuanYi Micro Hei Mono"
                           "PingFang SC" "Microsoft Yahei UI" "Simhei")
             when (font-available-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.05)))
                      (set-fontset-font t 'han (font-spec :family font))))

    ;; If your font supports ligatures, uncomment it
    (cl-loop for chars in '("::" "..." "->" "=>" "<=" ">=" "!==" "!=" "===" "==")
             for key = (aref chars 0)
             do (set-char-table-range
                 composition-function-table  key
                 (nconc (char-table-range composition-function-table key)
                        `(,(vector (regexp-quote chars) 0 'font-shape-gstring)))))
    ))

(add-hook 'window-setup-hook #'setup-fonts)
(add-hook 'server-after-make-frame-hook #'setup-fonts)

;;; Fringes
;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like diff-hl and flycheck.
(setq-default indicate-buffer-boundaries nil
              indicate-empty-lines nil)

;; Child frame
(use-package posframe
  :ensure t
  :custom-face
  (child-frame-border ((t (:inherit posframe-border))))
  :hook (after-load-theme . posframe-delete-all)
  :init
  (defface posframe-border
    `((t (:inherit region)))
    "Face used by the `posframe' border."
    :group 'posframe)
  (defvar posframe-border-width 2
    "Default posframe border width.")
  :config
  (with-no-warnings
    (defun aiser/posframe--prettify-frame (&rest _)
      (set-face-background 'fringe nil posframe--frame))
    (advice-add #'posframe--create-posframe :after #'aiser/posframe--prettify-frame)

    (defun posframe-poshandler-frame-center-near-bottom (info)
      (cons (/ (- (plist-get info :parent-frame-width)
                  (plist-get info :posframe-width))
               2)
            (/ (+ (plist-get info :parent-frame-height)
                  (* 2 (plist-get info :font-height)))
               2)))))

(provide 'init-base)

;;; init-base.el ends here
