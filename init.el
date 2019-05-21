;;

;; -------------------------------------------------------------------------------
;; package initialize
;; -------------------------------------------------------------------------------
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(when (>= emacs-major-version 25)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://elpa.emacs-china.org/melpa/") t)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(company-auto-complete t)
 '(company-auto-complete-chars nil)
 '(company-idle-delay 0.08)
 '(company-minimum-prefix-length 2)
 '(counsel-bookmark-avoid-dired nil)
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(package-selected-packages
   (quote
    (company-c-headers company-flx company-math company-shell company-statistics company counsel swiper))))

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

;; -------------------------------------------------------------------------------
;; customize face
;; -------------------------------------------------------------------------------
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "outline" :slant normal :weight bold :height 158 :width normal))))
 '(company-scrollbar-bg ((t (:background "lime green"))))
 '(company-scrollbar-fg ((t (:background "gray97"))))
 '(company-template-field ((t (:background "green" :foreground "black"))))
 '(company-tooltip ((t (:background "lawn green" :foreground "black"))))
 '(swiper-line-face ((t (:inherit highlight)))))

(tool-bar-mode -1)
;; (setq inhibit-splash-screen t)
(global-linum-mode t)

(require 'recentf)
(recentf-mode t)
(setq recentf-max-menu-items 25)

;; -------------------------------------------------------------------------------
;; common configs
;; -------------------------------------------------------------------------------
(setq make-backup-files nil)
(delete-selection-mode t)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; -------------------------------------------------------------------------------
;; shortcut
;; -------------------------------------------------------------------------------
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f8>") 'open-my-init-file)
(global-set-key (kbd "M-\\") 'split-window-horizontally)
(global-set-key (kbd "C-x M-\\") 'split-window-vertically)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;;(setq search-default-mode #'char-fold-to-regexp)
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
