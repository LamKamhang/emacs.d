;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun my-install-popular-themes (popular-themes)
  "Install POPULAR-THEMES from melpa."
  (dolist (theme popular-themes)
    (require-package theme)))

;; most popular 100 themes
(my-install-popular-themes
 '(
   atom-one-dark-theme
   color-theme-sanityinc-solarized
   color-theme-sanityinc-tomorrow
   dracula-theme
   material-theme
   monokai-theme
   spacegray-theme
   spacemacs-theme
   ))
;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; If you don't customize it, this is the theme you get.
;; (setq-default custom-enabled-themes '(dracula))
(setq-default custom-enabled-themes '(tango-dark))
(custom-set-faces `(region ((t (:background "#555753" :foreground "white")))))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun tango_dark_theme ()
  "Activate a tango dark theme."
  (interactive)
  (setq custom-enabled-themes '(tango-dark))
  (custom-set-faces `(region ((t (:background "#555753" :foreground "white")))))
  (reapply-themes))

(defun dracula_theme ()
  "Activate a dracula color theme."
  (interactive)
  (setq custom-enabled-themes '(dracula))
  (custom-set-faces `(show-paren-match ((t (:background "#EE7785" :foreground "black" :weight bold)))))
  (custom-set-faces `(region ((t (:background "#555753" :foreground "white")))))
  (reapply-themes))

(when (maybe-require-package 'dimmer)
  (setq-default dimmer-fraction 0.15)
  (add-hook 'after-init-hook 'dimmer-mode)
  (after-load 'dimmer
    ;; TODO: file upstream as a PR
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all))))
  (after-load 'dimmer
    ;; Don't dim in terminal windows. Even with 256 colours it can
    ;; lead to poor contrast.  Better would be to vary dimmer-fraction
    ;; according to frame type.
    (defun sanityinc/display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p)))


(provide 'init-themes)
;;; init-themes.el ends here
