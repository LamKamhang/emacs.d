;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'dracula-theme)
(require-package 'monokai-theme)

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; If you don't customize it, this is the theme you get.
;; (setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))
(setq-default custom-enabled-themes '(dracula))
(custom-set-faces `(show-paren-match ((t (:background "#EE7785" :foreground "black" :weight bold)))))
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
(defun monokai_theme ()
  "Activate a monokai theme."
  (interactive)
  (setq custom-enabled-themes '(monokai))
  (reapply-themes))

(defun tomorrow_bright_theme ()
  "Activate a tomorror bright theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))

(defun tsdh_dark_theme ()
  "Activate a tsdh dark theme."
  (interactive)
  (setq custom-enabled-themes '(tsdh-dark))
  (reapply-themes))

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
  ;; TODO: file upstream as a PR
  (after-load 'dimmer
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))))


(provide 'init-themes)
;;; init-themes.el ends here
