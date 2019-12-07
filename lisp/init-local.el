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

(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.imp\\'" . c++-mode))

(provide 'init-local)
;;; init-local.el ends here
