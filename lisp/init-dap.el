;;; init-dap.el --- modeline,dashboard and icons -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
;; Left and right side windows occupy full frame height
(use-package emacs :custom (window-sides-vertical t))

;; For a more ergonomic Emacs and `dape' experience
(use-package repeat :custom (repeat-mode +1))

(use-package
  dape
  :ensure t
  :bind ("<f5>" . dape)
  :custom (dape-buffer-window-arrangment 'right)
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
     ("i" "Info"              dape-info)]))

(provide 'init-dap)
;;; init-dap.el ends here
