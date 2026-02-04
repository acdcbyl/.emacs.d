;;; init-dired.el --- Some tweaks for dired -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;;Dired beautification and enhancement
(use-package
  dired
  :config
  (setq
   dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  (put 'dired-find-alternate-file 'disabled nil))

(use-package
  dirvish
  :ensure t
  :init (dirvish-override-dired-mode)
  :custom (dirvish-side-width 30)
  ;; (dirvish-window-fringe 0)
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/" "Home")
     ("d" "~/Downloads/" "Downloads")
     ("m" "/mnt/" "Drives")
     ("s" "/ssh:my-remote-server")
     "SSH server"
     ("e" "/sudo:root@localhost:/etc")
     "Modify program settings"
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  :config
  (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq
   dirvish-attributes ; The order *MATTERS* for some attributes
   '(vc-state subtree-state
              nerd-icons
              collapse
              git-msg
              file-time
              file-size)
   dirvish-side-attributes '(vc-state nerd-icons collapse file-size))
  ;; open large directory (over 20000 files) asynchronously with `fd' command
  (setq dirvish-large-directory-threshold 20000)
  (setq
   dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group --time-style=iso")
  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("h" . dired-up-directory) ; So you can adjust `dired' bindings here
   ("?" . dirvish-dispatch) ; [?] a helpful cheatsheet
   ("a" . dirvish-setup-menu) ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f" . dirvish-file-info-menu) ; [f]ile info
   ("o" . dirvish-quick-access) ; [o]pen `dirvish-quick-access-entries'
   ("s" . dirvish-quicksort) ; [s]ort flie list
   ("r" . dirvish-history-jump) ; [r]ecent visited
   ("l" . dirvish-ls-switches-menu) ; [l]s command flags
   ("v" . dirvish-vc-menu) ; [v]ersion control commands
   ("*" . dirvish-mark-menu)
   ("y" . dirvish-yank-menu)
   ("N" . dirvish-narrow)
   ("^" . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))

(provide 'init-dired)

;;; init-dired.el ends here.
