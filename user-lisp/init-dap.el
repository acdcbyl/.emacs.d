;;; init-dap.el --- debug  -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:
;;

;;; Code:
;; Left and right side windows occupy full frame height
(use-package emacs :custom (window-sides-vertical t))

(require 'transient)
(require 'init-icons)
(require 'init-evil)
(use-package
  dape
  :ensure t
  :bind ("<f5>" . dape)
  :custom (dape-buffer-window-arrangement 'right)
  :config
  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))
  (transient-define-prefix dape-transient ()
    "Dape Menu"
    ["Base"
     ("c" "Continue"          dape-continue)
     ("n" "Next (Step over)"  dape-next)
     ("i" "Step in"           dape-step-in)
     ("o" "Step out"          dape-step-out)
     ("p" "Pause"             dape-pause)
     ("r" "Restart"           dape-restart)
     ("q" "Quit"              dape-quit :transient nil)]
    ["Breakpoint"
     ("b" "Toggle breakpoint" dape-breakpoint-toggle)
     ("B" "Remove all"        dape-breakpoint-remove-all)
     ("l" "Log breakpoint"    dape-breakpoint-log)]
    ["Evaluate/Watch"
     ("e" "Evaluate"          dape-evaluate-expression)
     ("w" "Watch"             dape-watch-dwim)
     ("s" "Select stack"      dape-select-stack)]
    ["Others"
     ("i" "Info"              dape-info)])
  :general
  (aiser/leader-def
    "d"   (list :wk (format "%s debug" (nerd-icons-mdicon "nf-md-bug")))
    "dd"  'dape
    "dq"  'dape-quit
    "dr"  'dape-restart
    "db"  'dape-breakpoint-toggle
    "dB"  'dape-breakpoint-remove-all
    "dn"  'dape-next
    "di"  'dape-step-in
    "do"  'dape-step-out
    "dc"  'dape-continue
    "dp"  'dape-pause
    "de"  'dape-evaluate-expression
    "ds"  'dape-select-stack
    "dw"  'dape-watch-dwim
    "dh"  'dape-transient))

(provide 'init-dap)
;;; init-dap.el ends here
