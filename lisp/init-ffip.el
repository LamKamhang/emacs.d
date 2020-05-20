;;; init-ffip.el --- find file in project -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'find-file-in-project)
(eval-after-load 'find-file-in-project
  '(progn
     (defun my-search-git-reflog-code ()
       (let* ((default-directory (locate-dominating-file default-directory ".git")))
         (ffip-shell-command-to-string (format "git --no-pager reflog --date=short -S\"%s\" -p"
                                               (read-string "Regex: ")))))
     (push 'my-search-git-reflog-code ffip-diff-backends)
     (setq ffip-match-path-instead-of-filename t)))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let* ((project-dir (ffip-get-project-root-directory))
         (file-name (buffer-file-name)))
    (if project-dir
        (progn
          (neotree-dir project-dir)
          (neotree-find file-name))
      (message "Could not find git project root."))))

;;navigate tree
(when (require-package 'neotree)
  ;; (add-to-list 'load-path "/some/path/neotree")
  (global-set-key [f12] 'neotree-toggle))

(provide 'init-ffip)
;;; init-ffip.el ends here
