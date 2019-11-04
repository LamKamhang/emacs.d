;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'flycheck)
  ;;(add-hook 'after-init-hook 'global-flycheck-mode)
  (add-hook 'prog-mode-hook #'flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  (when (maybe-require-package 'flycheck-color-mode-line)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

  (add-hook
   'c++mode-hook
   (lambda ()
     (setq flycheck-gcc-language-standard "c++11")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (with-eval-after-load 'flycheck ;;
;;   (flycheck-pos-tip-mode))      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-flycheck)
;;; init-flycheck.el ends here
