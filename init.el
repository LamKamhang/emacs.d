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
;;

;;; Code:

;; ------------------------------------------------------------------------------
;; package initialize
;; ------------------------------------------------------------------------------
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(when (>= emacs-major-version 25)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://elpa.emacs-china.org/melpa/") t)
  (package-initialize)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(company-auto-complete t)
 '(company-auto-complete-chars "")
 '(company-c-headers-path-system
   (quote
    ("/usr/include/" "/usr/local/include/" "/usr/include/c++/7/")))
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(counsel-bookmark-avoid-dired nil)
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(display-time-mode t)
 '(fringe-mode 0 nil (fringe))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (origami company-c-headers company flycheck-pos-tip fuzzy rust-mode ace-window gh-md markdown-mode cmake-mode flycheck ggtags yasnippet-snippets yasnippet smartparens highlight-parentheses counsel swiper)))
 '(scroll-bar-mode nil)
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
 '(default ((t (:family "Ubuntu Mono" :foundry "outline" :slant normal :weight bold :height 158 :width normal))))
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))
 '(swiper-line-face ((t (:inherit highlight)))))



(if (eq window-system 'w32)
    (custom-set-faces
     '(default ((t (:family #("新宋体" 0 3 (charset chinese-gbk)) :foundry "outline" :slant normal :weight normal :height 143 :width normal)))))
  (custom-set-faces
   '(default ((t (:family "Ubuntu Mono" :foundry "outline" :slant normal :weight bold :height 158 :width normal)))))
  )


(display-time-mode t)
(tool-bar-mode 0)
(setq inhibit-splash-screen t)
;;(global-linum-mode t)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'text-mode-hook 'linum-mode)

(setq company-show-numbers t)

(require 'recentf)
(require 'origami)
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

;; Configure network proxy
(setq my-proxy "web-proxy.tencent.com:8080")
(defun show-proxy ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current proxy is \"%s\"" my-proxy)
    (message "No proxy")))

(defun set-proxy ()
  "Set http/https proxy."
  (interactive)
  (setq url-proxy-services `(("http" . ,my-proxy)
                             ("https" . ,my-proxy)))
  (show-proxy))

(defun unset-proxy ()
  "Unset http/https proxy."
  (interactive)
  (setq url-proxy-services nil)
  (show-proxy))

(defun toggle-proxy ()
  "Toggle http/https proxy."
  (interactive)
  (if url-proxy-services
      (unset-proxy)
    (set-proxy)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
