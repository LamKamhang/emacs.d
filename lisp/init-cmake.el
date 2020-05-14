;;; init-cmake.el --- cmake config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'cmake-font-lock)
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

(provide 'init-cmake)
;;; init-cmake.el ends here
