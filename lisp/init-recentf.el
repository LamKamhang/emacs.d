;;; init-recentf.el --- Settings for tracking recent files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-hook 'after-init-hook 'recentf-mode)
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude '("/tmp/" "/ssh:"))

(global-set-key (kbd "C-c C-r") 'recentf-open-files)

(provide 'init-recentf)
;;; init-recentf.el ends here
