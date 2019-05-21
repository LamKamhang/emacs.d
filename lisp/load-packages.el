(require 'cl)
(defvar lamkamhang/packages package-selected-packages)

(defun lamkamhang/packages-installed-p ()
  (loop for pkg in lamkamhang/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (lamkamhang/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg lamkamhang/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(provide 'load-packages)
