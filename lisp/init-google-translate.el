;;; init-google-translate.el --- google translate config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package google-translate
  :ensure t
  :config
  ;; (setq google-translate-backend-method 'curl)
  (setq google-translate-base-url "https://translate.google.cn/translate_a/single")
  (setq google-translate--tkk-url "https://translate.google.cn/")
  (setq google-translate-default-source-language "auto")
  (setq google-translate-default-target-language "zh-CN")
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
   (bind-keys*
    ("C-c t" . google-translate-at-point)
    ("C-c r" . google-translate-at-point-reverse)
    ("C-c T" . google-translate-query-translate)
    ("C-c R" . google-translate-query-translate-reverse)
    ))

(provide 'init-google-translate)
;;; init-google-translate.el ends here
