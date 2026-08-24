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
(setq org-directory "~/Org/") ; Non-absolute paths for agenda and
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

;; NOTE: org-roam has been replaced by vulpea; no org-roam variables
;; are needed here any more.

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
  (require 'org-tempo) ; <s template expansion
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

;; Show hidden markup (bold/italic markers etc.) while editing
(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))

;; Paste/screenshot images into org notes (saved to images/ subdir)
(use-package org-download
  :ensure t
  :after org
  :config
  (setq org-download-method 'directory
        org-download-image-dir "images"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 3: extensions (vulpea, etc.)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vulpea
  :ensure t
  :after org
  :commands
  (
   vulpea-insert
   vulpea-find
   vulpea-db-sync-full-scan
   vulpea-db-query
   vulpea-db-query-by-tags-some
   vulpea-db-query-by-tags-every
   vulpea-db-query-by-tags-none
   vulpea-db-query-by-property
   vulpea-db-query-by-links-some
   vulpea-db-query-by-links-every
   vulpea-db-query-dead-links
   vulpea-db-query-orphan-notes
   vulpea-db-query-isolated-notes
   vulpea-db-query-title-collisions)
  :init
  ;; --------------------------------------------------------------------------
  ;; Note directories
  ;; --------------------------------------------------------------------------
  ;;
  ;; Put every directory containing your Org notes here.
  ;;
  ;; Example:
  ;;
  ;;   ~/org/
  ;;   ~/org/projects/
  ;;   ~/org/reference/
  ;;
  ;; Vulpea recursively indexes these directories.
  ;;
  (setq vulpea-db-sync-directories
        '("~/Org/"))

  ;; --------------------------------------------------------------------------
  ;; Database
  ;; --------------------------------------------------------------------------
  ;;
  ;; Keep the database outside the notes directory.
  ;;
  ;; This is especially useful if your Org directory is managed by Git,
  ;; Syncthing, Dropbox, etc.
  ;;
  (setq vulpea-db-location
        (expand-file-name "vulpea.db"
                          user-emacs-directory))

  ;; --------------------------------------------------------------------------
  ;; Heading-level notes
  ;; --------------------------------------------------------------------------
  ;;
  ;; t = index Org headings with IDs as notes.
  ;;
  ;; nil = only index file-level notes.
  ;;
  ;; Heading-level indexing is more powerful but requires more parsing.
  ;;
  (setq vulpea-db-index-heading-level t)

  ;; --------------------------------------------------------------------------
  ;; Note creation template
  ;; --------------------------------------------------------------------------
  ;;
  ;; File names are slugified titles, no timestamp prefix (the default
  ;; `${timestamp}_${slug}.org' was deemed noise).  Caveat: identical
  ;; titles produce identical file names; `aiser/vulpea-create' guards
  ;; against silently overwriting an existing note.
  ;;
  (setq vulpea-create-default-template '(:file-name "${slug}.org"))

  ;; --------------------------------------------------------------------------
  ;; External filesystem monitoring
  ;; --------------------------------------------------------------------------
  ;;
  ;; fswatch is recommended if your notes can be changed outside Emacs:
  ;;
  ;;   git pull
  ;;   Syncthing
  ;;   Dropbox
  ;;   shell scripts
  ;;   other editors
  ;;
  ;; Vulpea v2 uses an asynchronous architecture, so synchronization
  ;; doesn't block normal Emacs interaction.
  ;;
  (setq vulpea-db-sync-external-method 'fswatch)

  ;; --------------------------------------------------------------------------
  ;; Debugging
  ;; --------------------------------------------------------------------------
  ;;
  ;; Keep this nil normally.
  ;;
  ;; Set to t temporarily when debugging synchronization.
  ;;
  (setq vulpea-db-sync-debug nil)

  ;; --------------------------------------------------------------------------
  ;; Async worker extraction (v2)
  ;; --------------------------------------------------------------------------
  ;;
  ;; 'full = parse AND write the DB in a background emacs --batch worker
  ;; (database switches to WAL journaling).  The UI only registers note
  ;; IDs, so syncs never freeze the save path.
  ;;
  (setq vulpea-db-async-extraction 'full)

  ;; --------------------------------------------------------------------------
  ;; Initial scan on autosync enable
  ;; --------------------------------------------------------------------------
  ;;
  ;; Scan asynchronously when autosync starts, catching changes made
  ;; while Emacs was closed (git pull, external edits).
  ;;
  (setq vulpea-db-sync-scan-on-enable 'async)

  ;; Silence routine sync status messages on every save.
  (setq vulpea-db-sync-verbose nil)


  :config

  ;; --------------------------------------------------------------------------
  ;; Start automatic synchronization
  ;; --------------------------------------------------------------------------
  ;;
  ;; This enables Vulpea's filesystem watcher / async synchronization.
  ;;
  (vulpea-db-autosync-mode 1)
  ;; Record the pre-save title so `vulpea-propagate-title-change'
  ;; doesn't need to ask for the old title.
  (vulpea-title-change-detection-mode 1))

(use-package vulpea-ui
  :ensure t
  :after vulpea
  )

;; ----------------------------------------------------------------------------
;; Interactive wrappers for the non-interactive v2 API
;; ----------------------------------------------------------------------------
;; `vulpea-create' and `vulpea-select' are programmatic interfaces
;; (no (interactive) spec), so they can't be bound or called via M-x.

(defun aiser/vulpea-create (title)
  "Create a new vulpea note with TITLE and visit it for editing."
  (interactive "sNote title: ")
  ;; `vulpea--create-file' writes directly without a collision check, so
  ;; an existing file with the same slug would be silently overwritten.
  ;; Refuse instead.
  (let ((path (ignore-errors (vulpea--expand-file-name-template title))))
    (when (and path (file-exists-p path))
      (user-error "Note file already exists: %s" path)))
  (let ((note (vulpea-create title)))
    (when note
      (vulpea-visit note))))

(defun aiser/vulpea-select-other-window ()
  "Select a vulpea note and open it in another window."
  (interactive)
  (let ((note (vulpea-select "Note")))
    (when note
      (vulpea-visit note t))))

;; ----------------------------------------------------------------------------
;; Evil keybindings: SPC v prefix (via general)
;; ----------------------------------------------------------------------------
(aiser/leader-def
  "v"   (list :wk (format "%s vulpea" (nerd-icons-mdicon "nf-md-note_text_outline")))
  "vf"  'vulpea-find
  "vF"  'consult-vulpea-find
  "vg"  'consult-vulpea-grep
  "vi"  'vulpea-insert
  "vc"  'aiser/vulpea-create
  "vs"  'aiser/vulpea-select-other-window
  "vr"  'vulpea-propagate-title-change
  "vm"  'vulpea-move-file
  "vt"  'vulpea-ui-sidebar-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convenience functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aiser/vulpea-sync ()
  "Run a complete Vulpea database scan."
  (interactive)
  (vulpea-db-sync-full-scan))

(defun aiser/vulpea-check-database ()
  "Run basic diagnostics against the Vulpea database."
  (interactive)
  (message
   "Dead links: %d, orphan notes: %d, isolated notes: %d"
   (length (vulpea-db-query-dead-links))
   (length (vulpea-db-query-orphan-notes))
   (length (vulpea-db-query-isolated-notes))))

(use-package consult-vulpea
  :ensure t
  :after vulpea
  :config
  (consult-vulpea-mode 1))

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
