;;; init.el --- The main entry for emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (< emacs-major-version 29)
  (error
   (format
    "Only works with Emacs 29 and newer; you have version ~a"
    emacs-major-version)))

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

;; Keep ~/.emacs.d/ clean.
(use-package no-littering :ensure t :demand t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   load files
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir)))

(require 'init-base)
(require 'init-utils)
(require 'init-ui)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in customization framework
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load a private file for secrets, if it exists
(let ((private-file (locate-user-emacs-file "lisp/private.el")))
  (when (file-exists-p private-file)
    (load private-file)))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((qml-ts-mode :url "https://github.com/darcamo/qml-ts-mode.git"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
