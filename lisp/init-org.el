;;; init-org.el --- Org mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These variables need to be set for Org-mode's full power to be unlocked!
;;;
;;; You can read the documentation for any variable with `C-h v'. If you have
;;; Consult configured (see the `base.el' file) then it should help you find
;;; what you're looking for.

;;; Phase 1 variables
;; Make load Org files quickly
(setq org-element-use-cache t)
(setq org-element-cache-persistent t)

;;; Phase 2 variables
;; Tell Emacs to use Hunspell, and specify the dictionary directory (if it can't be found by default)
(setq ispell-program-name "hunspell")

;; Specify the default dictionary (change it to what you want, e.g., en_US, zh_CN, etc.)
(setq ispell-dictionary "en_US")
;; Agenda variables
(setq org-directory "~/Documents/org/") ; Non-absolute paths for agenda and
                                        ; capture templates will look here.

(setq org-agenda-files '("inbox.org" "work.org" "gongkao.org" "index.org"))

;; Default tags
(setq org-tag-alist
      '(
        ;; locale
        (:startgroup)
        ("home" . ?h)
        ("work" . ?w)
        ("school" . ?s)
        (:endgroup)
        (:newline)
        ;; scale
        (:startgroup)
        ("one-shot" . ?o)
        ("project" . ?j)
        ("tiny" . ?t)
        (:endgroup)
        ;; misc
        ("meta")
        ("review")
        ("reading")))

;; Org-refile: where should org-refile look?
(setq org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 3)))

;;; Phase 3 variables

;; Org-roam variables
(setq org-roam-directory "~/Documents/org-roam/")
(setq org-roam-index-file "~/Documents/org-roam/index.org")

;;; Optional variables

;; Advanced: Custom link types
;; This example is for linking a person's 7-character ID to their page on the
;; free genealogy website Family Search.
(setq org-link-abbrev-alist
      '(("family_search"
         .
         "https://www.familysearch.org/tree/person/details/%s")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 1: editing and exporting files
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
  org
  :hook
  ((org-mode . visual-line-mode) ; wrap lines at word breaks
   (org-mode . flyspell-mode)) ; spell checking!

  :bind
  (:map
   global-map
   ("C-c l s" . org-store-link) ; Mnemonic: link → store
   ("C-c l i" . org-insert-link-global)) ; Mnemonic: link → insert
  :config
  (require 'oc-csl) ; citation support
  (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)

  ;; Instead of just two states (TODO, DONE) we set up a few different states
  ;; that a task can be in.
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "WAITING(w@/!)"
           "STARTED(s!)"
           "|"
           "DONE(d!)"
           "OBSOLETE(o@)")))

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)

  (setq
   org-capture-templates
   '(("c"
      "Default Capture"
      entry
      (file "inbox.org")
      "* TODO %?\n%U\n%i")
     ;; Capture and keep an org-link to the thing we're currently working with
     ("r"
      "Capture with Reference"
      entry
      (file "inbox.org")
      "* TODO %?\n%U\n%i\n%a")
     ;; Define a section
     ("w" "Work")
     ("wm"
      "Work meeting"
      entry
      (file+headline "work.org" "Meetings")
      "** TODO %?\n%U\n%i\n%a")
     ("wr"
      "Work report"
      entry
      (file+headline "work.org" "Reports")
      "** TODO %?\n%U\n%i\n%a")))

  (setq org-agenda-custom-commands
        '(("n" "Agenda and All Todos" ((agenda) (todo)))
          ("w" "Work" agenda "" ((org-agenda-files '("work.org"))))
          ("g" "考公备考"
           ((agenda
             ""
             ((org-agenda-files '("gongkao.org"))
              (org-agenda-overriding-header "📚 考公每日任务")))
            (todo
             "TODO"
             ((org-agenda-files '("gongkao.org"))
              (org-agenda-overriding-header "📚 考公待办")))))))
  :general
  (aiser/leader-def
    "a"   (list :wk (format "%s app" (nerd-icons-mdicon "nf-md-apps")))
    "aa"  'org-agenda
    "oc"  'org-capture
    "ol"  'org-store-link)

  (aiser/localleader-def
    :major-modes '(org-mode)
    :keymaps 'org-mode-map
    "." 'org-goto
    "a" 'org-archive-subtree
    "d" 'org-deadline
    "e" 'org-set-effort
    "f" 'org-footnote-action
    "l" 'org-lint
    "o" 'org-toggle-ordered-property
    "p" 'org-set-property
    "q" 'org-set-tags-command
    "r" 'org-refile
    "s" 'org-schedule
    "t" 'org-todo
    "T" 'org-todo-list
    ;; babel
    "bp" 'org-babel-previous-src-block
    "bn" 'org-babel-next-src-block
    "be" 'org-babel-expand-src-block
    "bg" 'org-babel-goto-named-src-block
    "bs" 'org-babel-execute-subtree
    "bb" 'org-babel-execute-buffer
    "bt" 'org-babel-tangle
    "bf" 'org-babel-tangle-file
    "bc" 'org-babel-check-src-block
    "bi" 'org-babel-insert-header-arg
    "bI" 'org-babel-view-src-block-info
    "bk" 'org-babel-remove-result-one-or-many
    ;; clock
    "cc" 'org-clock-in
    "cC" 'org-clock-out
    "cd" 'org-clock-mark-default-task
    "ce" 'org-clock-modify-effort-estimate
    "cg" 'org-clock-goto
    "cl" 'org-clock-in-last
    "cr" 'org-clock-report
    "cs" 'org-clock-display
    "cx" 'org-clock-cancel
    "c=" 'org-clock-timestamps-up
    "c-" 'org-clock-timestamps-down
    ;; insert
    "Id" 'org-insert-drawer
    "In" 'org-add-note
    "It" 'org-time-stamp-inactive
    "Ii" 'org-toggle-inline-images
    "IT" 'org-time-stamp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 3: extensions (org-roam, etc.)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package
;;   org-roam
;;   :ensure t
;;   :defer t
;;   :config (org-roam-db-autosync-mode)
;;   ;; Dedicated side window for backlinks
;;   (add-to-list
;;    'display-buffer-alist
;;    '("\\*org-roam\\*"
;;      (display-buffer-in-side-window)
;;      (side . right)
;;      (window-width . 0.4)
;;      (window-height . fit-window-to-buffer))))

;; Pretty web interface for org-roam
                                        ;(use-package org-roam-ui
                                        ;  :ensure t
                                        ;  :after org-roam
                                        ;  :config
                                        ;  (setq org-roam-ui-sync-theme t
                                        ;        org-roam-ui-follow t
                                        ;        org-roam-ui-update-on-save t
                                        ;        org-roam-ui-open-on-start t))

;; Install and configure org-modern
(use-package
  org-modern
  :ensure t
  :defer t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-star 'replace)
  ;; ◉⦿⊚⊙○
  ;;ⅠⅡⅢⅣⅤⅥ
  ;; ♠♤♥♡♣♧
  (org-modern-replace-stars "◉⦿⊚⊙∘"))

(provide 'init-org)
;;; init-org.el ends here
