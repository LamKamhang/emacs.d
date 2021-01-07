;;; init-glsl.el --- ace window config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'glsl-mode)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.tesc\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.tese\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.comp\\'" . glsl-mode))

(require-package 'company-glsl)
(when (executable-find "glslangValidator")
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-glsl)))


(provide 'init-glsl)
;;; init-glsl.el ends here
