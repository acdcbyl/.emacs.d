;;; init.el --- The main entry for emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (< emacs-major-version 29)
  (error
   (format
    "Only works with Emacs 29 and newer; you have version ~a"
    emacs-major-version)))

;; Set custom file
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Manual package initialization
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package initialization
;;
;; We'll stick to the built-in GNU and non-GNU ELPAs (Emacs Lisp Package
;; Archive) for the base install, but there are some other ELPAs you could look
;; at if you want more packages. MELPA in particular is very popular. See
;; instructions at:
;;
;;    https://melpa.org/#/getting-started
;;
;; You can simply uncomment the following if you'd like to get started with
;; MELPA packages quickly:
;;
(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
               t)
  (add-to-list 'package-archives
               '("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
               t)
  (add-to-list 'package-archives
               '("nongnu"
                 .
                 "https://mirrors.ustc.edu.cn/elpa/nongnu/")
               t))

;; If you want to turn off the welcome screen, uncomment this
                                        ;(setopt inhibit-splash-screen t)
;; Enable build when using vc
(setq package-vc-allow-build-commands t)

;; Keep ~/.emacs.d/ clean.
(use-package no-littering :ensure t :demand t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   load files
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "lang" dir))))

(require 'init-base)
(require 'init-icons)
(require 'init-utils)
(require 'init-windows)
(require 'init-dired)
(require 'init-dashboard)
(require 'init-ui)
(require 'init-pdf)
(require 'init-dev)
(require 'init-dap)
(require 'init-completion)
(require 'init-music)
(require 'init-feed)
(require 'init-workspaces)
(require 'init-org)
(require 'init-evil)
;; if you don't want to use `use-package :vc'
;; (require 'init-package)

;;; init.el ends here
