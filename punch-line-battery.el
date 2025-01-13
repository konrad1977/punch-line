;;; punch-line-battery.el --- A customized mode-line for Emacs, with status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs,
;; configurable colors, and the ability to customize displayed information.

;;; Code:

(require 'battery)
(require 'nerd-icons)

(unless (bound-and-true-p battery-status-function)
  (battery-update-handler))

(defgroup punch-battery nil
  "Customization group for punch-line-battery."
  :group 'punch-line)

(defcustom punch-battery-show-percentage t
  "When non-nil, display battery percentage as text after the icon."
  :type 'boolean
  :group 'punch-battery)

(defcustom punch-show-battery-info t
  "If set to t, show battery icons with nerdicons."
  :type 'boolean
  :group 'punch-battery)

(defcustom punch-battery-cache-update-interval 60
  "Interval in seconds for updating the Battery- cache."
  :type 'number
  :group 'punch-line)

(defvar punch-battery-info-cache nil
  "Cache for Battery information.")

(defvar punch-battery-info-cache-time 0
  "Time of last cache update.")

(defun punch-battery-create-info ()
  "Create battery percentage or charging status using text and nerd-font icons."
  (when (and punch-show-battery-info
             (bound-and-true-p display-battery-mode))
    (let* ((battery-plist (funcall battery-status-function))
           (percentage (string-to-number (battery-format "%p" battery-plist)))
           (status (battery-format "%B" battery-plist))
           (charging (or (string= status "AC") (string= status "charging")))
           (icon (cond
                  (charging (nerd-icons-faicon "nf-fa-plug"))
                  ((>= percentage 87.5) (nerd-icons-faicon "nf-fa-battery"))
                  ((>= percentage 62.5) (nerd-icons-faicon "nf-fa-battery_3"))
                  ((>= percentage 37.5) (nerd-icons-faicon "nf-fa-battery_2"))
                  ((>= percentage 12.5) (nerd-icons-faicon "nf-fa-battery_1"))
                  (t (nerd-icons-faicon "nf-fa-battery_0"))))
           (face (cond
                  (charging 'success)
                  ((<= percentage 10) 'error)
                  ((<= percentage 30) 'warning)
                  (t 'success)))
           (percentage-text (if punch-battery-show-percentage
                                (format " %d%%" percentage)
                              "")))
      (if (and percentage status)
          (propertize
           (format "%s %s%%"
                   icon
                   percentage-text)
           'face face)
        "No battery info"))))

(defun punch-battery-info ()
  "Return battery information, updating the cache if necessary."
  (let ((current-time (float-time)))
    (when (or (null punch-battery-info-cache)
	      (> (- current-time punch-battery-info-cache-time) punch-battery-cache-update-interval))
      (setq punch-battery-info-cache-time current-time)
      (setq punch-battery-info-cache (punch-battery-create-info)))
      punch-battery-info-cache))

(provide 'punch-line-battery)
;;; punch-line-battery.el ends here
