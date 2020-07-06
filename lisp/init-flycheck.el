;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'flycheck)
  ;;(add-hook 'after-init-hook 'global-flycheck-mode)
  (add-hook 'prog-mode-hook #'flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  ;; (setq-default flycheck-highlighting-mode 'lines)
  (when (maybe-require-package 'flycheck-color-mode-line)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

;;; flycheck-pkg-config plugin
(require-package 'flycheck-pkg-config)
(with-eval-after-load 'flycheck
  (diminish 'flycheck-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
