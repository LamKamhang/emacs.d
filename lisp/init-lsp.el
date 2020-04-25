;;; init-lsp.el --- Language Service Protocol config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'lsp-mode)
(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-auto-guess-root t)
  :hook
  ((prog-mode) . lsp)
  (lsp-mode . lsp-enable-which-key-integration))
;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "C-c l")

(require-package 'lsp-ui)
(use-package lsp-ui :commands lsp-ui-mode)

(require-package 'lsp-ivy)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(require-package 'dap-mode)
(use-package dap-mode)

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

(provide 'init-lsp)
;;; init-lsp.el ends here
