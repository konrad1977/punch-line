;; punch-line-package.el - punch-line - A minimal mode line for Emacs -*- lexical-binding: t; -*-
;;; Code:
(require 'package)
(require 'cl-lib)
(require 'nerd-icons)

(defface punch-line-package-update-face
  '((t :inherit mode-line-emphasis))
  "Face for displaying package update information in the mode line.")

(defcustom punch-show-package-info t
  "When non-nil, show package update information in the mode line."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-package-cache-time 3600
  "Time in seconds to cache package update information (default: 1 hour)."
  :type 'integer
  :group 'punch-line)

(defvar punch-package-updates-count 0
  "Cached number of packages that have updates available.")

(defvar punch-package-last-check-time nil
  "Time at which the package updates were last checked.")

(defvar punch-package-updates-finished-hook nil
  "Hook run after package updates check completes. Receives the count as argument.")

(defun punch-package-check-updates-callback (upgradables)
  "Callback function to handle the result of the async package check."
  (setq punch-package-updates-count (length upgradables))
  (setq punch-package-last-check-time (current-time))
  (message "Package check completed. %d packages can be upgraded." punch-package-updates-count))

(defun punch-package-check-updates ()
  "Return the number of upgradable packages asynchronously."
  (async-start
   `(lambda ()
      (require 'package)
      (package-initialize)
      (package-refresh-contents)
      (let* ((installed-packages (mapcar #'car package-alist))
             (upgradable-count 0))
        (dolist (pkg installed-packages)
          (let* ((desc (cadr (assq pkg package-archive-contents)))
                 (installed-pkg (cadr (assq pkg package-alist)))
                 (installed-version (and installed-pkg
                                      (if (fboundp 'package-desc-version)
                                          (package-desc-version installed-pkg)
                                        (package-desc-vers installed-pkg))))
                 (newest-version (and desc
                                    (if (fboundp 'package-desc-version)
                                        (package-desc-version desc)
                                      (package-desc-vers desc)))))
            (when (and installed-version newest-version
                       (version-list-< installed-version newest-version))
              (setq upgradable-count (1+ upgradable-count))
              (message "Package %s can be upgraded from %s to %s"
                       pkg
                       (package-version-join installed-version)
                       (package-version-join newest-version)))))
        upgradable-count))
   (lambda (result)
     (message "Package update check completed. Found %d upgradable packages" result)
     result)))

(defun punch-package-upgradable-packages ()
  "Return the number of upgradable packages, using cached value if available."
  (when (or (null punch-package-last-check-time)
            (> (float-time (time-subtract (current-time) punch-package-last-check-time))
               punch-package-cache-time))
    (punch-package-check-updates))
  punch-package-updates-count)

(defun punch-package-info ()
  "Return a string with package update information."
  (when punch-show-package-info
    (let ((updates (punch-package-upgradable-packages)))
      (if (and (numberp updates) (> updates 0))
          (propertize (format "%s %d" (nerd-icons-codicon "nf-cod-package") updates)
                      'face 'punch-line-package-update-face)
        ""))))

(provide 'punch-line-package)
;;; punch-line-package.el ends here
