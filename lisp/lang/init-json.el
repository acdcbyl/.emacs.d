;;; init-json.el --- JSON configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; NOTE: the third-party `json-mode' package has been removed.  It
;; registered auto-mode-alist with the mode symbol `json-mode', which
;; never matches the built-in remap entry `(js-json-mode .
;; json-ts-mode)' (major-mode remapping is an exact symbol lookup), so
;; .json files stayed on the old regex-based mode.  The built-in
;; json-ts-mode (remapped from js-json-mode) handles .json now.
;;
;; Self-healing guard: if the package ever gets reinstalled (e.g. via
;; `package-install-selected-packages'), its autoload hijacks
;; auto-mode-alist again at package-init time; drop those entries so
;; the remap to json-ts-mode keeps working.

;; The built-in mode only covers \.json\'; handle .jsonld the same way
;; (via js-json-mode so the treesit remap applies uniformly).
(add-to-list 'auto-mode-alist '("\\.jsonld\\'" . js-json-mode))

(provide 'init-json)
;;; init-json.el ends here
