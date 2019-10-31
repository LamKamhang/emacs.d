;;; init.el --- my config file for emacs

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
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

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

;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(company-auto-complete t)
 '(company-auto-complete-chars "")
 '(company-c-headers-path-system
   (quote
    ("/usr/include/" "/usr/local/include/" "/usr/include/c++/7/")))
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(counsel-bookmark-avoid-dired nil)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" . t)))
 '(display-time-mode t)
 '(fringe-mode 0 nil (fringe))
 '(package-selected-packages
   (quote
    (uptimes dotenv-mode daemons dsvn htmlize lua-mode gnuplot flycheck-ledger ledger-mode origami regex-tool info-colors flycheck-clojure cider elein cljsbuild-mode clojure-mode slime-company hippie-expand-slime slime cask-mode cl-libify flycheck-package highlight-quoted macrostep cl-lib-highlight aggressive-indent immortal-scratch auto-compile ipretty elisp-slime-nav paredit-everywhere paredit nginx-mode company-terraform terraform-mode docker-compose-mode dockerfile-mode docker yaml-mode toml-mode flycheck-rust racer rust-mode sqlformat projectile-rails yard-mode bundler goto-gem yari robe ruby-compilation inf-ruby rspec-mode ruby-hash-syntax psci psc-ide purescript-mode flycheck-elm elm-test-runner elm-mode dhall-mode reformatter dante haskell-mode company-anaconda anaconda-mode pip-requirements restclient httprepl haml-mode css-eldoc skewer-less sass-mode rainbow-mode tagedit org-pomodoro writeroom-mode org-cliplink company-php smarty-mode php-mode add-node-modules-path skewer-mode js-comint prettier-js typescript-mode coffee-mode js2-mode json-mode erlang csv-mode markdown-mode textile-mode cmd-to-echo alert ibuffer-projectile github-review forge github-clone bug-reference-github yagist git-commit magit-todos magit git-timemachine gitconfig-mode gitignore-mode git-blamed vc-darcs darcsum browse-at-remote whitespace-cleanup-mode which-key highlight-escape-sequences whole-line-or-region move-dup page-break-lines multiple-cursors avy expand-region browse-kill-ring symbol-overlay rainbow-delimiters goto-line-preview beacon mode-line-bell vlf list-unicode-display unfill mmm-mode session switch-window company-quickhelp company ivy-xref swiper projectile counsel ivy smex flycheck-color-mode-line flycheck ibuffer-vc wgrep anzu diff-hl diredfl disable-mouse default-text-scale dimmer dracula-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized command-log-mode scratch diminish exec-path-from-shell gnu-elpa-keyring-update fullframe seq less-css-mode neotree company-c-headers flycheck-pos-tip fuzzy ace-window gh-md cmake-mode ggtags yasnippet-snippets yasnippet smartparens highlight-parentheses)))
 '(scroll-bar-mode nil)
 '(session-use-package t nil (session))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(require 'cl)

(defvar lamkamhang/packages package-selected-packages)

(defun lamkamhang/packages-installed-p ()
  (loop for pkg in lamkamhang/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (lamkamhang/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg lamkamhang/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; ------------------------------------------------------------------------------
;; customize face
;; ------------------------------------------------------------------------------
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 143 :width normal))))
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight normal)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight normal)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))
 '(swiper-line-face ((t (:inherit highlight)))))

(if (eq window-system 'w32)
    (custom-set-faces
     '(default ((t (:family #("新宋体" 0 3 (charset chinese-gbk)) :foundry "outline" :slant normal :weight normal :height 143 :width normal)))))
  )

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require-package 'diminish)
(maybe-require-package 'scratch)
(require-package 'command-log-mode)

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
;;(require 'init-helm)
(require 'init-hippie-expand)
(require 'init-company)
(require 'init-windows)
(require 'init-sessions)
(require 'init-mmm)

(require 'init-editing-utils)
(require 'init-whitespace)

(require 'init-vc)
(require 'init-darcs)
(require 'init-git)
(require 'init-github)

(require 'init-projectile)

(require 'init-compile)
;;(require 'init-crontab)
(require 'init-textile)
(require 'init-markdown)
(require 'init-csv)
(require 'init-erlang)
(require 'init-javascript)
(require 'init-php)
(require 'init-org)
(require 'init-nxml)
(require 'init-html)
(require 'init-css)
(require 'init-haml)
(require 'init-http)
(require 'init-python)
(require 'init-haskell)
(require 'init-elm)
(require 'init-purescript)
(require 'init-ruby)
(require 'init-rails)
(require 'init-sql)
(require 'init-rust)
(require 'init-toml)
(require 'init-yaml)
(require 'init-docker)
(require 'init-terraform)
;;(require 'init-nix)
(maybe-require-package 'nginx-mode)

(require 'init-paredit)
(require 'init-lisp)
(require 'init-slime)
(require 'init-clojure)
(require 'init-clojure-cider)
(require 'init-common-lisp)

(require 'init-proxy)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)

(require 'init-folding)
(require 'init-dash)

;;(require 'init-twitter)
;; (require 'init-mu)
(require 'init-ledger)
;; Extra packages which don't require any configuration

(require-package 'gnuplot)
(require-package 'lua-mode)
(require-package 'htmlize)
(require-package 'dsvn)
(when *is-a-mac*
  (require-package 'osx-location))
(unless (eq system-type 'windows-nt)
  (maybe-require-package 'daemons))
(maybe-require-package 'dotenv-mode)

(when (maybe-require-package 'uptimes)
  (setq-default uptimes-keep-count 200)
  (add-hook 'after-init-hook (lambda () (require 'uptimes))))

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)





















;;(global-linum-mode t)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'text-mode-hook 'linum-mode)

(setq company-show-numbers t)

(require 'recentf)
;; (require 'origami)
(recentf-mode t)
(setq recentf-max-menu-items 25)

(global-hl-line-mode t)
;; ------------------------------------------------------------------------------
;; common configs
;; ------------------------------------------------------------------------------
(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(delete-selection-mode t)
(global-auto-revert-mode t)

(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'text-mode-hook 'company-mode)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "SPC") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (add-to-list 'company-backends 'company-c-headers))

(add-hook
 'c++mode-hook
 (lambda ()
   (setq flycheck-gcc-language-standard "c++11")))

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

(add-hook
 'eshell-mode-hook
 (lambda ()
   (setq pcomplete-cycle-completions nil)))

(defun my-eshell-remove-pcomplete ()
  (remove-hook 'completion-at-point-functions #'pcomplete-completions-at-point t))
(add-hook 'eshell-mode-hook #'my-eshell-remove-pcomplete)

;;(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook #'flycheck-mode)

(add-hook 'prog-mode-hook #'yas-minor-mode)
;;(require 'yasnippet)
;;(yas-global-mode 1)

(require 'smartparens-config)
(add-hook 'after-init-hook #'smartparens-global-mode)

;;highlight parentheses
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;;navigate tree
(add-to-list 'load-path "/some/path/neotree")
(require 'neotree)
(global-set-key [f12] 'neotree-toggle)

;;project manager
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; ------------------------------------------------------------------------------
;; shortcut
;; ------------------------------------------------------------------------------
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f8>") 'open-my-init-file)
(global-set-key (kbd "M-|") 'split-window-horizontally)
(global-set-key (kbd "C-M-|") 'split-window-vertically)

(defun split-balance-window-horizontally ()
  (interactive)
  (command-execute 'split-window-horizontally)
  (command-execute 'balance-windows)
  )

(defun split-balance-window-vertically ()
  (interactive)
  (command-execute 'split-window-vertically)
  (command-execute 'balance-windows)
  )

(global-set-key (kbd "M-\\") 'split-balance-window-horizontally)
(global-set-key (kbd "C-M-\\") 'split-balance-window-vertically)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
(setq search-default-mode 'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(global-set-key (kbd "C-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
