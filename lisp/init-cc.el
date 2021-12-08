;;; init-cc.el --- c-like language config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst *my-cpp-include-path* (quote
                                 ("."
                                  "/usr/include/"
                                  "/usr/local/include/"
                                  "/usr/include/c++/10/"
                                  "/usr/include/eigen3/"
                                  "/opt/intel/mkl/include/"
                                  "/opt/intel/ipp/include/"
                                  "/opt/intel/tbb/include/"
                                  "/opt/intel/daal/include/"
                                  "/opt/intel/pstl/include/")))

(require-package 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(with-eval-after-load 'flycheck
  (add-hook
   'c++mode-hook
   (lambda ()
     (setq flycheck-gcc-language-standard "c++17"))))

(with-eval-after-load 'flycheck
  (add-hook
   'c++-mode-hook
   (lambda () (setq flycheck-clang-include-path
                *my-cpp-include-path*))))

(require-package 'company-c-headers)
(with-eval-after-load 'company-c-headers
  (setq-default company-c-headers-path-system
                *my-cpp-include-path*))
;; (with-eval-after-load 'company
;;   (add-to-list 'company-backends `(company-capf company-c-headers)))
(defun my-cpp-highlight ()
  "Highlight c/c++ #if 0 #endif macros."
  ;; (interactive)
  (setq cpp-known-face 'default)
  (setq cpp-unknown-face 'default)
  (setq cpp-known-writable 't)
  (setq cpp-unknown-writable 't)
  (setq cpp-edit-list '(("0" font-lock-comment-face default both)
                        ("1" default font-lock-comment-face both)))
  (cpp-highlight-buffer t))

(add-hook 'c-mode-common-hook 'my-cpp-highlight)
(add-hook 'after-save-hook 'my-cpp-highlight)

(provide 'init-cc)
;;; init-cc.el ends here
