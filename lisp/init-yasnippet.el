;;; init-yasnippet.el --- yet another snippet config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (require-package 'yasnippet)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'text-mode-hook #'yas-minor-mode)
  ;; {{ modes do NOT inherit from prog-mode
  (add-hook 'cmake-mode-hook #'yas-minor-mode)
  (add-hook 'web-mode-hook #'yas-minor-mode)
  (add-hook 'scss-mode-hook #'yas-minor-mode)
  ;; }}
  (require-package 'yasnippet-snippets)

  (when (maybe-require-package 'ivy-yasnippet)
    (global-set-key (kbd "C-c i") 'ivy-yasnippet))

  (after-load 'yasnippet
    (yas-reload-all)))

(defun my-yas-reload-all ()
  "Compile and reload yasnippets.  Run the command after adding new snippets."
  (interactive)
  (yas-compile-directory (file-truename "~/.emacs.d/snippets"))
  (yas-reload-all)
  (yas-minor-mode 1))

(after-load 'yasnippet
  (diminish 'yas-minor-mode))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
