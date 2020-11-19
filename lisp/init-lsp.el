;;; init-lsp.el --- Language Service Protocol config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (setq lsp-auto-guess-root nil
        lsp-file-watch-threshold 10000
        company-idle-delay 0.0
        lsp-idle-delay 0.1)
  (setq lsp-file-watch-ignored
        (append lsp-file-watch-ignored
                (list "[/\\\\]build-release$"
                      "[/\\\\]build-debug$"
                      "[/\\\\]build$"
                      "[/\\\\]external$"
                      "[/\\\\]3rd$"
                      "[/\\\\]packages$")))
  (setq lsp-clients-clangd-executable "clangd")
  :custom
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (lsp-keymap-prefix "C-c l")
  :hook
  ((c-mode c++-mode cuda-mode objc-mode) . lsp)
  (lsp-mode . lsp-enable-which-key-integration))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode
  :ensure t)

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

(provide 'init-lsp)
;;; init-lsp.el ends here
