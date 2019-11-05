;;; init-local.el --- local config for emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; common configs
;;----------------------------------------------------------------------------

;; (add-hook
;;  'eshell-mode-hook
;;  (lambda ()
;;    (setq pcomplete-cycle-completions nil)))

;; (defun my-eshell-remove-pcomplete ()
;;   "Fix eshell complete error."
;;   (remove-hook 'completion-at-point-functions #'pcomplete-completions-at-point t))
;; (add-hook 'eshell-mode-hook #'my-eshell-remove-pcomplete)

;; (add-hook 'prog-mode-hook #'yas-minor-mode)

(provide 'init-local)
;;; init-local.el ends here
