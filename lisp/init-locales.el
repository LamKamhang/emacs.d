;;; init-locales.el --- Configure default locale -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun sanityinc/locale-var-encoding (v)
  "Return the encoding portion of the locale string V, or nil if missing."
  (when v
    (save-match-data
      (let ((case-fold-search t))
        (when (string-match "\\.\\([^.]*\\)\\'" v)
          (intern (downcase (match-string 1 v))))))))

(dolist (varname '("LC_ALL" "LANG" "LC_CTYPE"))
  (let ((encoding (sanityinc/locale-var-encoding (getenv varname))))
    (unless (memq encoding '(nil utf8 utf-8))
      (message "Warning: non-UTF8 encoding in environment variable %s may cause interop problems with this Emacs configuration." varname))))

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))

;; (defun sanityinc/utf8-locale-p (v)
;;   "Return whether locale string V relates to a UTF-8 locale."
;;   (and v (string-match-p "UTF-8" v)))

;; (defun sanityinc/locale-is-utf8-p ()
;;   "Return t iff the \"locale\" command or environment variables prefer UTF-8."
;;   (or (sanityinc/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
;;       (sanityinc/utf8-locale-p (getenv "LC_ALL"))
;;       (sanityinc/utf8-locale-p (getenv "LC_CTYPE"))
;;       (sanityinc/utf8-locale-p (getenv "LANG"))))

;; (when (or window-system (sanityinc/locale-is-utf8-p))
;;   (set-language-environment 'utf-8)
;;   (setq locale-coding-system 'utf-8)
;;   (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
;;   (prefer-coding-system 'utf-8))

(provide 'init-locales)
;;; init-locales.el ends here
