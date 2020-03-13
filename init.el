;;; init.el --- my config file for emacs -*- lexical-binding: t -*-

;; Author: LamKamhang

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)

(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Emacs is too old -- this config requires v%s or higher" minver)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *win64* (eq system-type 'windows-nt))
(defconst *cygwin* (eq system-type 'cygwin) )
(defconst *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(defconst *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)))
(defconst *no-memory* (cond
                       (*is-a-mac*
                        ;; @see https://discussions.apple.com/thread/1753088
                        ;; "sysctl -n hw.physmem" does not work
                        (<= (string-to-number (shell-command-to-string "sysctl -n hw.memsize"))
                            (* 4 1024 1024)))
                       (*linux* nil)
                       (t nil)))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(defun require-init (pkg &optional maybe-disabled)
  "Load PKG if MAYBE-DISABLED is nil or it's nil but start up in normal slowly."
  (when (or (not maybe-disabled) (not (boundp 'startup-now)))
    (load (file-truename (format "~/.emacs.d/lisp/%s" pkg)) t t)))

(require-init 'init-autoload)
;; (require-init 'init-modeline)
(require 'init-utils)
;; (require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require-package 'diminish)
(maybe-require-package 'scratch)
(require-package 'command-log-mode)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(defconst *scroll-length* 2)
(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flycheck)

(require 'init-recentf)
(require 'init-smex)
(require 'init-ivy)
(require 'init-hippie-expand)
(require 'init-company)
(require 'init-windows)
(require 'init-sessions)
(require 'init-mmm)

(require 'init-editing-utils)
(require 'init-whitespace)

(require 'init-vc)
(require 'init-git)
(require 'init-github)

(require 'init-projectile)

(require 'init-compile)
(require 'init-markdown)
(require 'init-org)
(require 'init-latex)
(require 'init-csv)

(require 'init-cc)
(require 'init-gud)
(require 'init-gtags)
(require 'init-ctags)
(require 'init-python)
(require 'init-rust)

(require 'init-term-mode)
(require 'init-nxml)
(require 'init-html)
(require 'init-css)
(require 'init-javascript)
(require 'init-http)
(require 'init-sql)
(require 'init-yaml)
(require 'init-docker)

(require 'init-paredit)
(require 'init-lisp)
(require 'init-slime)
(require 'init-common-lisp)

(require 'init-proxy)
(require 'init-yasnippet)

(require 'init-misc)
(require 'init-ffip)

(require 'init-folding)
(require 'init-modeline)

(require 'init-ledger)

;; Extra packages which don't require any configuration
(when *is-a-mac*
  (require-package 'osx-location))
(unless (eq system-type 'windows-nt)
  (maybe-require-package 'daemons))
(maybe-require-package 'dotenv-mode)

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(require 'init-local nil t)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
