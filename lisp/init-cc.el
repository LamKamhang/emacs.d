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

(after-load 'flycheck
  (add-hook
   'c++mode-hook
   (lambda ()
     (setq flycheck-gcc-language-standard "c++11"))))

(after-load 'flycheck
  (add-hook
   'c++-mode-hook
   (lambda () (setq flycheck-clang-include-path
                *my-cpp-include-path*))))

(after-load 'company-c-headers
  (setq-default company-c-headers-path-system
                *my-cpp-include-path*))

;; (require-package 'lsp-mode)
;; (use-package lsp-mode
;;   :commands lsp
;;   :ensure t
;;   :config
;;   (setq lsp-auto-guess-root t)
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;          (lambda () (require 'ccls) (lsp))))
;; (require-package 'lsp-ui)
;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :ensure t)
;; (require-package 'lsp-ivy)
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; (require-package 'company-lsp)
;; (use-package company-lsp
;;   :ensure t
;;   :after (company lsp-mode)
;;   :commands company-lsp
;;   :config (push 'company-lsp company-backends)) ;; add company-lsp as a backend
;; (require-package 'ccls)
;; (use-package ccls
;;   :ensure t
;;   :config
;;   (setq ccls-executable "ccls")
;;   (setq ccls-args '("--log-file=/tmp/ccls.log"))
;;   ;; (setq lsp-prefer-flymake nil)
;;   ;; (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
;;   ;; :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;   ;;        (lambda () (require 'ccls) (lsp)))
;;   )
(require-package 'ivy-xref)
(use-package ivy-xref
  :ensure t
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(defun c-wx-lineup-topmost-intro-cont (langelem)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "EVT_" (line-end-position) t)
        'c-basic-offset
      (c-lineup-topmost-intro-cont langelem))))

;; c-default-style: https://www.gnu.org/software/emacs/manual/html_node/ccmode/Built_002din-Styles.html
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux"))
      )

(defun fix-c-indent-offset-according-to-syntax-context (key val)
  ;; remove the old element
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new value
  (add-to-list 'c-offsets-alist '(key . val)))

(defun my-common-cc-mode-setup ()
  "setup shared by all languages (java/groovy/c++ ...)"
  (setq c-basic-offset 2)
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)

  ;; syntax-highlight aggressively
  ;; (setq font-lock-support-mode 'lazy-lock-mode)
  (setq lazy-lock-defer-contextually t)
  (setq lazy-lock-defer-time 0)

                                        ;make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)

  ;; indent
  ;; google "C/C++/Java code indentation in Emacs" for more advanced skills
  ;; C code:
  ;;   if(1) // press ENTER here, zero means no indentation
  (fix-c-indent-offset-according-to-syntax-context 'substatement 0)
  ;;   void fn() // press ENTER here, zero means no indentation
  (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0))

(defun my-c-mode-setup ()
  "C/C++ only setup."
  ;; @see http://stackoverflow.com/questions/3509919/ \
  ;; emacs-c-opening-corresponding-header-file
  (local-set-key (kbd "C-x C-o") 'ff-find-other-file)

  (setq cc-search-directories '("." "/usr/include" "/usr/local/include/*" "../*/include" "$WXWIN/include"))

  ;; {{ @see https://github.com/redguardtoo/cpputils-cmake
  ;; In theory, you can write your own Makefile for `flyamke-mode' without cmake.
  ;; Nobody actually does it in real world.
  ;; So make sure cmake is used before uncomment below code.

  ;; (when buffer-file-name
  ;;   (flymake-mode 1)
  ;;   (when (and (executable-find "cmake")
  ;;              (not (string-match-p "^\\(/usr/local/include\\|/usr/src/linux/include\\)/.*"
  ;;                                   buffer-file-name)))
  ;;     (cppcm-reload-all)))

  ;; }}

  ;; wxWidgets setup
  (c-set-offset 'topmost-intro-cont 'c-wx-lineup-topmost-intro-cont)

  ;; debugging Emacs c code
  (add-to-list 'imenu-generic-expression '(nil "^DEFUN *(\"\\([a-zA-Z0-9-]+\\)" 1))

  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft))))

;; donot use c-mode-common-hook or cc-mode-hook because many major-modes use this hook
(defun c-mode-common-hook-setup ()
  (unless (is-buffer-file-temp)
    (my-common-cc-mode-setup)
    (unless (or (derived-mode-p 'java-mode) (derived-mode-p 'groovy-mode))
      (my-c-mode-setup))

    ;; gtags (GNU global) stuff
    (when (and (executable-find "global")
               ;; `man global' to figure out why
               (not (string-match-p "GTAGS not found"
                                    (shell-command-to-string "global -p"))))
      ;; emacs 24.4+ will set up eldoc automatically.
      ;; so below code is NOT needed.
      (eldoc-mode 1))))
(add-hook 'c-mode-common-hook 'c-mode-common-hook-setup)


(provide 'init-cc)
;;; init-cc.el ends here
