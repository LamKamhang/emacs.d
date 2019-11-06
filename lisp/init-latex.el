;;; init-latex.el --- latex config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'auctex)
(require-package 'cdlatex)

(mapc (lambda (mode)
        (add-hook 'LaTeX-mode-hook mode)); with AUCTeX LaTeX mode
      (list 'turn-on-cdlatex
            'LaTeX-math-mode
            'turn-on-reftex
            'outline-minor-mode
            'auto-fill-mode
            'flyspell-mode
            'hide-body
            t))

(require-package 'company-auctex)
(company-auctex-init)

(provide 'init-latex)

;;; init-latex.el ends here
