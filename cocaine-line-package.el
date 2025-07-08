;; cocaine-line-package.el - cocaine-line - A minimal mode line for Emacs -*- lexical-binding: t; -*-
;;; Code:
(require 'package)
(require 'cl-lib)
(require 'nerd-icons)

(defface cocaine-line-package-update-face
  '((t :inherit mode-line-emphasis))
  "Face for displaying package update information in the mode line.")

(defcustom cocaine-show-package-info t
  "When non-nil, show package update information in the mode line."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-package-cache-time 3600
  "Time in seconds to cache package update information (default: 1 hour)."
  :type 'integer
  :group 'cocaine-line)

(defvar cocaine-package-updates-count 0
  "Cached number of packages that have updates available.")

(defvar cocaine-package-last-check-time nil
  "Time at which the package updates were last checked.")

(defvar cocaine-package-updates-finished-hook nil
  "Hook run after package updates check completes. Receives the count as argument.")

(defun cocaine-package-check-updates-callback (upgradables)
  "Callback function to handle the result of the async package check."
  (setq cocaine-package-updates-count (length upgradables))
  (setq cocaine-package-last-check-time (current-time))
  (message "Package check completed. %d packages can be upgraded." cocaine-package-updates-count))

(defun cocaine-package-check-updates ()
  "Return the number of upgradable packages asynchronously."
  (when (fboundp 'async-start)
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
       result))))

(defun cocaine-package-upgradable-packages ()
  "Return the number of upgradable packages, using cached value if available."
  (when (or (null cocaine-package-last-check-time)
            (> (float-time (time-subtract (current-time) cocaine-package-last-check-time))
               cocaine-package-cache-time))
    (cocaine-package-check-updates))
  cocaine-package-updates-count)

(defun cocaine-package-info ()
  "Return a string with package update information."
  (when cocaine-show-package-info
    (let ((updates (cocaine-package-upgradable-packages)))
      (if (and (numberp updates) (> updates 0))
          (propertize (format "%s %d" (nerd-icons-codicon "nf-cod-package") updates)
                      'face 'cocaine-line-package-update-face)
        ""))))

(provide 'cocaine-line-package)
;;; cocaine-line-package.el ends here 