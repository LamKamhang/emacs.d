;;; init-packages.el --- install selected packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl)

(defvar lamkamhang/packages package-selected-packages)

(defun lamkamhang/packages-installed-p ()
  "Check whether each package is installed."
  (loop for pkg in lamkamhang/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (lamkamhang/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg lamkamhang/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(provide 'init-packages)
;;; init-packages.el ends here
