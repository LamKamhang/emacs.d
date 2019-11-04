;;; init-navigate.el --- Navigate Tree -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;navigate tree
(when (require-package 'neotree)
  (add-to-list 'load-path "/some/path/neotree")
  (global-set-key [f12] 'neotree-toggle))


(provide 'init-navigate)
;;; init-navigate.el ends here
