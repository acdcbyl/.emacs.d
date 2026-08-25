;;; early-init.el --- The before init entry for emacs -*- lexical-binding: t no-byte-compile: t-*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings for quick startup and convenience
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Startup speed, annoyance suppression
(setq
 ;; set a high value before initialization, and it should be reduced to a
 ;; proper value after init
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.3
 read-process-output-max (* 1 1024 1024))

;; Let startup activate packages itself: with `package-quickstart' t it
;; just loads the quickstart file (much faster than scanning elpa/).
;; Do NOT set `package-enable-at-startup' to nil here, that would skip
;; the quickstart load entirely.
(setq package-quickstart t)
;; Pin to the same location no-littering uses, so the auto-refresh
;; triggered by the very first package install (which happens BEFORE
;; no-littering itself gets loaded) lands in var/ instead of the
;; default root directory.
(setq package-quickstart-file
      (expand-file-name "var/package-quickstart.el" user-emacs-directory))
(make-directory (file-name-directory package-quickstart-file) t)

;; For the git version (renamed in Emacs 29+)
(setq native-comp-jit-compilation t
      package-native-compile t)

;; Emacs 31: don't start new async native compilations while on battery
;; power (no-op on machines where no battery is detected).  Plain setq
;; here: the variable is defined in comp-run.el which loads later, and
;; defcustom never overrides an already-bound value.
(setq native-comp-async-on-battery-power t)

;; Emacs 31: User Lisp directory.  Everything below ~/.emacs.d/user-lisp/
;; is recursively byte-compiled (lazily, by timestamp), scraped for
;;; ###autoload cookies into .user-lisp-autoloads.el, and added to
;; `load-path' at startup -- before the regular init file loads, which
;; is why these variables must be set here.
;; Personal libraries and git-cloned packages can just be dropped in;
;; VC dirs (.git etc.) are skipped via `user-lisp-ignored-directories'.
(setq user-lisp-directory (expand-file-name "user-lisp/" user-emacs-directory)
      user-lisp-auto-scrape t)

;; Speed up startup by disabling file-name-handler-alist temporarily
(defvar aiser--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun aiser/setup-gc ()
  "For the faster startup."
  (setq
   gc-cons-threshold (* 64 1024 1024)
   gc-cons-percentage 0.15
   read-process-output-max (* 1 1024 1024)
   ;; Restore file-name-handler-alist
   file-name-handler-alist aiser--file-name-handler-alist
   ;; Don’t compact font caches during GC.
   inhibit-compacting-font-caches t))
(add-hook 'after-init-hook #'aiser/setup-gc)

(setq byte-compile-warnings '(not obsolete))
(setq warning-minimum-level :error)
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Silence stupid startup message
(advice-add #'display-startup-echo-area-message :override #'ignore)
;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like diff-hl and flycheck.
(setq-default indicate-buffer-boundaries nil)

;; See alse https://emacs-china.org/t/fringe-face/20143/4
;; (setq window-divider-default-places t
;;       window-divider-default-bottom-width 1
;;       window-divider-default-right-width 1)
;; ;; Make sure new frames use window-divider
;; (add-hook 'before-make-frame-hook 'window-divider-mode)
;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)
(tool-bar-mode -1) ; All these tools are in the menu-bar anyway
;; Disable the menu bar
(menu-bar-mode -1)
;; For long file,See also https://emacs-china.org/t/topic/25811/9
(setq-default bidi-display-reordering nil)
(setq
 bidi-inhibit-bpa t
 long-line-threshold 1000
 large-hscroll-threshold 1000
 syntax-wholeline-max 1000)

(setq default-frame-alist
      '(
        ;; (fullscreen . maximized)
        ;; You can turn off scroll bars by uncommenting these lines:
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)

        ;; Setting the face in here prevents flashes of
        ;; color as the theme gets activated
        ;; (background-color . "#000000")
        (ns-appearance . dark)
        (ns-transparent-titlebar . t)))
;;; early-init.el ends here
