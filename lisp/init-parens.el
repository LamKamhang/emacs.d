;;; init-parens.el --- Smartparens Config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'smartparens-config)
(add-hook 'after-init-hook #'smartparens-global-mode)

;;highlight parentheses
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

(provide 'init-parens)
;;; init-parens.el ends here
