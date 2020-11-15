;;; init-cc.el --- c-like language config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst *my-cpp-include-path* (quote
                                 ("."
                                  "/usr/include/"
                                  "/usr/local/include/"
                                  "/usr/include/c++/7/"
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
     (setq flycheck-gcc-language-standard "c++11"))))

(with-eval-after-load 'flycheck
  (add-hook
   'c++-mode-hook
   (lambda () (setq flycheck-clang-include-path
                *my-cpp-include-path*))))

(require-package 'company-c-headers)
(with-eval-after-load 'company-c-headers
  (setq-default company-c-headers-path-system
                *my-cpp-include-path*))
(with-eval-after-load 'company
  (add-to-list 'company-backends `(company-capf company-c-headers)))

(provide 'init-cc)
;;; init-cc.el ends here
