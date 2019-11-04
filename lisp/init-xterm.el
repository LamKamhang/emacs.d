;;; init-xterm.el --- Integrate with terminals such as xterm -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-frame-hooks)

(defconst *scroll-length* 5)
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down *scroll-length*)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up *scroll-length*)))

(autoload 'mwheel-install "mwheel")

(defun sanityinc/console-frame-setup ()
  (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
  (mwheel-install))



(add-hook 'after-make-console-frame-hooks 'sanityinc/console-frame-setup)

(provide 'init-xterm)
;;; init-xterm.el ends here
