;;; init-completion.el --- Initialize completion configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Modern completion configuration.
;;

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Motion aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
  avy
  :ensure t
  :bind
  (("C-c j" . avy-goto-line) ("s-j" . avy-goto-char-timer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Power-ups: Embark and Consult
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Consult: Misc. enhanced commands
(use-package
  consult
  :ensure t
  :bind
  (
   ;; Drop-in replacements
   ("C-x b" . consult-buffer) ; orig. switch-to-buffer
   ("M-y" . consult-yank-pop) ; orig. yank-pop
   ;; Searching
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line) ; Alternative: rebind C-s to use
   ("M-s s" . consult-line) ; consult-line instead of isearch, bind
   ("M-s L" . consult-line-multi) ; isearch to M-s s
   ("M-s o" . consult-outline)
   ;; Isearch integration
   :map isearch-mode-map
   ("M-e" . consult-isearch-history) ; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
   ("M-s l" . consult-line) ; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi) ; needed by consult-line to detect isearch
   )
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

(use-package embark-consult :ensure t)

(use-package
  embark
  :ensure t
  :after (avy embark-consult)
  :bind (("C-c a" . embark-act)) ; bind this to an easy key to hit
  :init
  ;; Add the option to run embark when using avy
  (defun aiser/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'aiser/avy-action-embark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer and completion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico: better vertical completion for minibuffer commands
(use-package
  vertico
  :ensure t
  :init
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode)
  ;; Make `vertico-repeat' (SPC ') remember history across invocations
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package
  vertico-directory
  :after vertico
  :bind
  (:map vertico-map ("M-DEL" . vertico-directory-delete-word)))

;; Marginalia: annotations for minibuffer
(use-package marginalia :ensure t :config (marginalia-mode))
;; Popup completion-at-point
(use-package
  corfu
  :ensure t
  :init (global-corfu-mode)
  :custom (corfu-auto t) (corfu-auto-delay 0.15) (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-preselect 'prompt)
  (corfu-auto-trigger ".") ;; Custom trigger characters
  (corfu-quit-no-match 'separator) ;; or t
  :config
  ;; Remember frequently used candidates and show numeric indices
  (corfu-history-mode +1)
  (corfu-indexed-mode +1)
  ;; Persist corfu history across sessions via savehist
  (add-to-list 'savehist-additional-variables 'corfu-history)
  :bind
  (:map
   corfu-map
   ("TAB" . corfu-next)
   ([tab] . corfu-next)
   ("S-TAB" . corfu-previous)
   ("SPC" . corfu-insert-separator)
   ([backtab] . corfu-previous)))

;; Part of corfu
(use-package
  corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.1 . 0.1))
  (corfu-popupinfo-hide nil)
  :config (corfu-popupinfo-mode))

;; Pretty icons for corfu
(use-package
  nerd-icons-corfu
  :ensure t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Fancy completion-at-point functions; there's too much in the cape package to
;; configure here; dive in when you're comfortable!
(use-package
  cape
  :ensure t
  :defer t
  :init
  (add-to-list
   'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package
  eshell
  :init
  (defun aiser/setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . aiser/setup-eshell)))

;; Orderless: powerful completion style
(use-package
  orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Emacs 30+: inline "ghost text" preview of the top candidate,
;; complements Corfu. Only in programming buffers.
(add-hook 'prog-mode-hook #'completion-preview-mode)

;; Emacs 31: keep ghost text and Corfu's popup from fighting.
(defun aiser/completion-preview-inhibit-corfu-p ()
  "Inhibit the completion preview while Corfu's popup is visible."
  (and (bound-and-true-p corfu--frame)
       (frame-live-p corfu--frame)
       (frame-visible-p corfu--frame)))
(add-hook 'completion-preview-inhibit-functions
          #'aiser/completion-preview-inhibit-corfu-p)
;; Sort preview candidates the same way Corfu sorts its popup.
(setopt completion-preview-sort-function #'corfu-sort-length-alpha)

(provide 'init-completion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
