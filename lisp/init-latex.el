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

;; (require-package 'company-auctex)
;; (company-auctex-init)

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
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    ;; First element is a character, followed by a number of strings attached to  ;;
                    ;; this key.  When the string contains a question mark, this is where the     ;;
                    ;; cursor will be positioned after insertion of the string into the buffer.   ;;
                    ;; See ‘cdlatex-math-symbol-alist-default’ for an example.  Any entry defined ;;
                    ;; here will replace the corresponding entry of the default list.  The        ;;
                    ;; defaults implement 3 levels of symbols so far: Level 1 for greek letters   ;;
                    ;; and standard symbols, level 2 for variations of level 1, and level 3 for   ;;
                    ;; functions and opperators.                                                  ;;
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    ;; ( ?a  ("\\alpha"          ))
                    ;; ( ?A  ("\\forall"         "\\aleph"))
                    ))
            (setq cdlatex-math-modify-alist
                  '(
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    ;; Each element contains 6 items:                                         ;;
                    ;; 0. key:      The character that is the key for a the accent.           ;;
                    ;; 1. mathcmd:  The LaTeX command associated with the accent in math mode ;;
                    ;; 2. textcmd:  The LaTeX command associated with the accent in text mode ;;
                    ;; 3. type:     t   if command with argument (e.g. \tilde{a}).            ;;
                    ;;              nil if style (e.g. {\cal a}).                             ;;
                    ;; 4. rmdot:    t   if the dot on i and j has to be removed.              ;;
                    ;; 5. it        t   if italic correction is required.                     ;;
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    ))
            (setq cdlatex-command-alist
                  '(
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    ;; Each element of this list is again a list with the following items:       ;;
                    ;; 0. KEYWORD     The key that has to be typed into the text.                ;;
                    ;; 1. DOCSTRING   A documentation string, less than 60 characters long.      ;;
                    ;; 2. REPLACE     The text to be substituted for the keyword, if any.        ;;
                    ;; 3. HOOK        A function to be called.                                   ;;
                    ;; 4. ARGS        Optional list of arguments to the function.                ;;
                    ;; 5. TEXTFLAG    non-nil means this keyword command is active in textmode.  ;;
                    ;; 6. MATHFLAG    non-nil means this keyword command is active in math mode. ;;
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    ;; ("big{" "Insert \\big\\{ \\big\\}" "\\big\\{ ? \\big\\" cdlatex-position-cursor nil nil t)
                    ))
            ))

(provide 'init-latex)
;;; init-latex.el ends here
