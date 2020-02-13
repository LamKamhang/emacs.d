;;; init-latex.el --- latex config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'auctex)
(require-package 'cdlatex)

;; ;; change the prefix of outline mode to C-c e
(add-hook 'outline-minor-mode-hook
          (lambda () (local-set-key "\C-z"
                                outline-mode-prefix-map)))

(setq-default TeX-master nil) ;; deal with multiple tex files.

(mapc (lambda (mode)
        (add-hook 'LaTeX-mode-hook mode)); with AUCTeX LaTeX mode
      (list 'turn-on-cdlatex
            'turn-on-reftex
            'turn-on-auto-fill
            'turn-on-flyspell
            'outline-minor-mode
            'hide-body
            t))

(require-package 'company-auctex)
(company-auctex-init)

;; set XeTeX mode in TeX/LaTeX
(add-hook 'LaTeX-mode-hook
          (lambda()
            (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
            (setq TeX-command-default "XeLaTeX")
            (setq cdlatex-auto-help-delay 0.2)
            (setq TeX-save-query nil)
            (setq TeX-show-compilation t)
            (setq cdlatex-math-symbol-alist
                  '(
                    ;; ( ?a  ("\\alpha"          ))
                    ;; ( ?A  ("\\forall"         "\\aleph"))
                    ))
            (setq cdlatex-math-modify-alist
                  '(
                    ;; ( ?\.   "\\dot"               nil        t   t   nil )
                    ;; ( ?\:   "\\ddot"              nil        t   t   nil )
                    ))
            (setq cdlatex-command-alist
                  '(
                    ;; ("big{" "Insert \\big\\{ \\big\\}" "\\big\\{ ? \\big\\" cdlatex-position-cursor nil nil t)
                    ))
            ))

(provide 'init-latex)
;;; init-latex.el ends here
