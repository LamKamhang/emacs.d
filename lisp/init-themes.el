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
   afternoon-theme
   alect-themes
   ample-theme
   ample-zen-theme
   anti-zenburn-theme
   apropospriate-theme
   atom-one-dark-theme
   badwolf-theme
   base16-theme
   birds-of-paradise-plus-theme
   bubbleberry-theme
   busybee-theme
   cherry-blossom-theme
   clues-theme
   color-theme-sanityinc-solarized
   color-theme-sanityinc-tomorrow
   cyberpunk-theme
   dakrone-theme
   darkburn-theme
   darkmine-theme
   darkokai-theme
   darktooth-theme
   django-theme
   doom-themes
   dracula-theme
   espresso-theme
   exotica-theme
   eziam-theme
   farmhouse-theme
   flatland-theme
   flatui-theme
   gandalf-theme
   gotham-theme
   grandshell-theme
   gruber-darker-theme
   gruvbox-theme
   hc-zenburn-theme
   hemisu-theme
   heroku-theme
   inkpot-theme
   ir-black-theme
   jazz-theme
   jbeans-theme
   kaolin-themes
   leuven-theme
   light-soap-theme
   lush-theme
   madhat2r-theme
   majapahit-theme
   material-theme
   minimal-theme
   moe-theme
   molokai-theme
   monochrome-theme
   monokai-theme
   mustang-theme
   naquadah-theme
   noctilux-theme
   obsidian-theme
   occidental-theme
   oldlace-theme
   omtose-phellack-theme
   organic-green-theme
   phoenix-dark-mono-theme
   phoenix-dark-pink-theme
   planet-theme
   professional-theme
   purple-haze-theme
   railscasts-theme
   rebecca-theme
   reverse-theme
   seti-theme
   smyx-theme
   soft-charcoal-theme
   soft-morning-theme
   soft-stone-theme
   solarized-theme
   soothe-theme
   spacegray-theme
   spacemacs-theme
   subatomic-theme
   subatomic256-theme
   sublime-themes
   sunny-day-theme
   tango-2-theme
   tango-plus-theme
   tangotango-theme
   tao-theme
   toxi-theme
   twilight-anti-bright-theme
   twilight-bright-theme
   twilight-theme
   ujelly-theme
   underwater-theme
   white-sand-theme
   zen-and-art-theme
   zenburn-theme
   atom-dark-theme
   nord-theme
   zerodark-theme
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
  ;; TODO: file upstream as a PR
  (after-load 'dimmer
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))))

(provide 'init-themes)
;;; init-themes.el ends here
