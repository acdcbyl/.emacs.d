;;; early-init.el --- The before init entry for emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; /        |                                              /       \               /  |                          /  |
;;; $$$$$$$$/ _____  ____   ______   _______  _______       $$$$$$$  | ______   ____$$ | ______   ______   _______$$ |   __
;;; $$ |__   /     \/    \ /      \ /       |/       |      $$ |__$$ |/      \ /    $$ |/      \ /      \ /       $$ |  /  |
;;; $$    |  $$$$$$ $$$$  |$$$$$$  /$$$$$$$//$$$$$$$/       $$    $$</$$$$$$  /$$$$$$$ /$$$$$$  /$$$$$$  /$$$$$$$/$$ |_/$$/
;;; $$$$$/   $$ | $$ | $$ |/    $$ $$ |     $$      \       $$$$$$$  $$    $$ $$ |  $$ $$ |  $$/$$ |  $$ $$ |     $$   $$<
;;; $$ |_____$$ | $$ | $$ /$$$$$$$ $$ \_____ $$$$$$  |      $$ |__$$ $$$$$$$$/$$ \__$$ $$ |     $$ \__$$ $$ \_____$$$$$$  \
;;; $$       $$ | $$ | $$ $$    $$ $$       /     $$/       $$    $$/$$       $$    $$ $$ |     $$    $$/$$       $$ | $$  |
;;; $$$$$$$$/$$/  $$/  $$/ $$$$$$$/ $$$$$$$/$$$$$$$/        $$$$$$$/  $$$$$$$/ $$$$$$$/$$/       $$$$$$/  $$$$$$$/$$/   $$/

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
 read-process-output-max (* 10 1024 1024))

;; Defer package loading until after init
(setq package-enable-at-startup nil)

;; Speed up startup by disabling file-name-handler-alist temporarily
(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun my/setup-gc ()
  (setq
   gc-cons-threshold (* 100 1024 1024)
   gc-cons-percentage 0.3
   read-process-output-max (* 10 1024 1024)
   ;; Restore file-name-handler-alist
   file-name-handler-alist my--file-name-handler-alist
   ;; Don’t compact font caches during GC.
   inhibit-compacting-font-caches t))
(add-hook 'after-init-hook #'my/setup-gc)

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))
;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like diff-hl and flycheck.
(setq-default indicate-buffer-boundaries nil)

;; See alse https://emacs-china.org/t/fringe-face/20143/4
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
;; Make sure new frames use window-divider
(add-hook 'before-make-frame-hook 'window-divider-mode)
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
