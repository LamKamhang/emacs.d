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

(use-package ccls
  :ensure t
  :config
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))
(setq ccls-executable "ccls")
(setq ccls-args '("--log-file=/tmp/ccls.log"))
(defun ccls/callee () (interactive) (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
(defun ccls/caller () (interactive) (lsp-ui-peek-find-custom "$ccls/call"))
(defun ccls/vars (kind) (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
(defun ccls/base (levels) (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
(defun ccls/derived (levels) (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
(defun ccls/member (kind) (interactive) (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))

;; References w/ Role::Role
(defun ccls/references-read () (interactive)
       (lsp-ui-peek-find-custom "textDocument/references"
                                (plist-put (lsp--text-document-position-params) :role 8)))

;; References w/ Role::Write
(defun ccls/references-write ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
                           (plist-put (lsp--text-document-position-params) :role 16)))

;; References w/ Role::Dynamic bit (macro expansions)
(defun ccls/references-macro () (interactive)
       (lsp-ui-peek-find-custom "textDocument/references"
                                (plist-put (lsp--text-document-position-params) :role 64)))

;; References w/o Role::Call bit (e.g. where functions are taken addresses)
(defun ccls/references-not-call () (interactive)
       (lsp-ui-peek-find-custom "textDocument/references"
                                (plist-put (lsp--text-document-position-params) :excludeRole 32)))

;; ccls/vars ccls/base ccls/derived ccls/members have a parameter while others are interactive.
;; (ccls/base 1) direct bases
;; (ccls/derived 1) direct derived
;; (ccls/member 2) => 2 (Type) => nested classes / types in a namespace
;; (ccls/member 3) => 3 (Func) => member functions / functions in a namespace
;; (ccls/member 0) => member variables / variables in a namespace
;; (ccls/vars 1) => field
;; (ccls/vars 2) => local variable
;; (ccls/vars 3) => field or local variable. 3 = 1 | 2
;; (ccls/vars 4) => parameter

;; ;; References whose filenames are under this project
(with-eval-after-load 'lsp-ui
  (lsp-ui-peek-find-references nil (list :folders (vector (projectile-project-root))))
  )
;; (setq ccls-sem-highlight-method 'font-lock)
(setq ccls-sem-highlight-method 'overlay)

;; For rainbow semantic highlighting
(ccls-use-default-rainbow-sem-highlight)

;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'before-save-hook 'lsp-format-buffer))

(provide 'init-cc)
;;; init-cc.el ends here
