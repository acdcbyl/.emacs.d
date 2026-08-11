;;; init.el --- The main entry for emacs -*- lexical-binding: t no-byte-compile: t-*-
;;; Commentary:
;;; Code:

(when (< emacs-major-version 31)
  (error
   (format
    "Only works with Emacs 31 and newer; you have version ~a"
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
                                        ; (add-to-list 'load-path "/home/aiser/.emacs.d/lisp")

(require 'init-base)
(require 'init-icons)
(require 'init-theme)
(require 'init-modeline)
(require 'init-diff)
(require 'init-utils)
(require 'init-windows)
(require 'init-dired)
(require 'init-dashboard)
(require 'init-ui)
(require 'init-completion)
(require 'init-project)
(require 'init-workspaces)
(require 'init-evil)

(add-hook 'after-init-hook
          (lambda ()
            (require 'init-dap)
            (require 'init-emms)
            (require 'init-email)
            (require 'init-dev)
            (require 'init-check)
            ;; (require 'init-treemacs)
            ;; (require 'init-matrix)
            (require 'init-feed)
            ;; (require 'init-tramp-rpc)
            ;; (require 'tramp-rpc)
            (require 'init-rimel)
            (run-with-idle-timer 0.2 nil (lambda ()
                                           (require 'init-org)
                                           (require 'init-telega)
                                           (require 'init-workspaces-apps)
                                           (require 'init-pdf)))
            ))

;;; init.el ends here
