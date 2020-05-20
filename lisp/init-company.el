;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

(setq tab-always-indent 'complete)
(setq company-show-numbers t)
(add-to-list 'completion-styles 'initials t)

(when (maybe-require-package 'company)
  ;; Don't require match, so you can still move your cursor as expected.
  (setq company-require-match nil)
  ;; Align annotation to the right side.
  ;; (setq company-tooltip-align-annotations t)
  ;; No downcase when completion.
  (setq company-dabbrev-downcase nil)
  ;; Company should be case sensitive.
  (setq company-dabbrev-ignore-case nil)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.2)

  (add-hook 'after-init-hook 'global-company-mode)
  (after-load 'company
    ;; @see https://github.com/company-mode/company-mode/issues/348
    ;; (require-package 'company-c-headers)
    ;; (add-to-list 'company-backends 'company-c-headers)
    ;; (setq-default company-backends
    ;;               `((company-capf company-c-headers)
    ;;                 (company-files company-dabbrev company-keywords company-yasnippet)
    ;;                 company-nxml
    ;;                 company-css
    ;;                 company-capf
    ;;                 company-clang
    ;;                 company-xcode
    ;;                 company-cmake
    ;;                 company-eclim
    ;;                 company-semantic
    ;;                 ;;... other backends
    ;;                 ))

    ;; (diminish 'company-mode)
    (define-key company-active-map (kbd "M-/") 'company-other-backend)
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "SPC") nil)
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map (kbd "<return>") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)
    (define-key company-active-map (kbd "TAB") #'company-complete-selection)
    (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
    (setq-default company-dabbrev-other-buffers 'all
                  company-tooltip-align-annotations t))
  (when (maybe-require-package 'company-quickhelp)
    (add-hook 'after-init-hook 'company-quickhelp-mode)))

;; Suspend page-break-lines-mode while company menu is active
;; (see https://github.com/company-mode/company-mode/issues/416)
(after-load 'company
  (after-load 'page-break-lines
    (defvar-local sanityinc/page-break-lines-on-p nil)

    (defun sanityinc/page-break-lines-disable (&rest ignore)
      (when (setq sanityinc/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
        (page-break-lines-mode -1)))

    (defun sanityinc/page-break-lines-maybe-reenable (&rest ignore)
      (when sanityinc/page-break-lines-on-p
        (page-break-lines-mode 1)))

    (add-hook 'company-completion-started-hook 'sanityinc/page-break-lines-disable)
    (add-hook 'company-after-completion-hook 'sanityinc/page-break-lines-maybe-reenable)))

(provide 'init-company)
;;; init-company.el ends here
