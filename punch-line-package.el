;;; punch-line-package.el - punch-line - A minimal mode line for Emacs -*- lexical-binding: t; -*-
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

;; (defun punch-package-check-updates ()
;;   "Check for package updates asynchronously and cache the result."
;;   (async-start
;;    `(lambda ()
;;       (require 'package)
;;       (let ((upgradables nil))
;;         (with-temp-buffer
;;           (package-menu-mode)
;;           (when (fboundp 'package--ensure-package-menu-mode)
;;             (package--ensure-package-menu-mode))
;;           (let ((inhibit-message t))
;;             (message "Refreshing package contents...")
;;             (package-refresh-contents))
;;         (message "Generating package menu...")
;;           (package-menu--generate nil t)
;;           (message "Finding upgradable packages...")
;;           (setq upgradables (package-menu--find-upgrades)))
;;         (message "Found %d upgradable package(s)." (length upgradables))
;;         (list upgradables (length upgradables))))
;;    (lambda (result)
;;      (let ((upgradables (nth 0 result))
;;            (count (nth 1 result)))
;;        (message "Async result received: %d upgradable package(s)." count)
;;        (setq punch-package-updates-count count)
;;        (setq punch-package-last-check-time (current-time))))))

(defun punch-package-check-updates-callback (upgradables)
  "Callback function to handle the result of the async package check."
  (setq punch-package-updates-count (length upgradables))
  (setq punch-package-last-check-time (current-time))
  (message "Package check completed. %d packages can be upgraded." punch-package-updates-count))

(defun punch-package-check-updates ()
  "Return the number of upgradable packages asynchronously."
  (async-start
   ;; What to do in the child process
   (lambda ()
     (require 'package)
     (let ((upgradables nil))
       (with-temp-buffer
         (let ((inhibit-message t)) ; Suppress output
           (package-menu-mode)
           (when (fboundp 'package--ensure-package-menu-mode)
             (package--ensure-package-menu-mode))
           ;; Fetch the remote list of packages.
           (package-refresh-contents)
           (package-menu--generate nil t)
           (setq upgradables (package-menu--find-upgrades))))
       upgradables))
   ;; What to do when it finishes
   'punch-package-check-updates-callback))


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
