;;; init-ace-window.el --- ace window config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (require-package 'ace-window)
  (global-set-key (kbd "C-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(provide 'init-ace-window)
;;; init-ace-window.el ends here
