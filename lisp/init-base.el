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
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; undo-limit
(setq undo-limit 67108864) ; 64mb.
(setq undo-strong-limit 100663296) ; 96mb.
(setq undo-outer-limit 1006632960) ; 960mb.

;; Save history of minibuffer
(savehist-mode)

;; Save cursor place
(save-place-mode)

;; Remember projects for project.el
(setq project-remember-projects-on-exit t)

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control) ; You can use other modifiers here

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; y/n instead of yes/no
(setq use-short-answers t)

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


;; We won't set these, but they're good to know about
;;
(setopt indent-tabs-mode nil)
;; (setopt tab-width 4)

;; Misc. UI tweaks
(blink-cursor-mode -1) ; Steady cursor
(pixel-scroll-precision-mode) ; Smooth scrolling

;; Use common keystrokes by default
;; (cua-mode)

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; Fonts
(defun font-available-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("PragmataPro")
             when (font-available-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height 140))
    ;; Set mode-line font
    ;; (cl-loop for font in '("SF Mono" "Menlo" "SF Pro Display" "Helvetica")
    ;;          when (font-available-p font)
    ;;          return (progn
    ;;                  (set-face-attribute 'mode-line nil :family font :height 120)
    ;;                  (when (facep 'mode-line-active)
    ;;                    (set-face-attribute 'mode-line-active nil :family font :height 120))
    ;;                  (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Symbola" "Apple Symbols" "Segoe UI Symbol"  "Symbol")
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

    ;; Register nerd-icons PUA codepoints so icons render in all contexts (which-key, etc.)
    (when (fboundp 'nerd-icons-set-font)
      (nerd-icons-set-font))

    ;; Re-register ClockFace after nerd-icons (remapped to PUA-A)
    (when (fboundp 'aiser/lambda-line-clockface-setup)
      (aiser/lambda-line-clockface-setup))
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
