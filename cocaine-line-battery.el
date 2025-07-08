;;; cocaine-line-battery.el --- A customized mode-line for Emacs, with status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs,
;; configurable colors, and the ability to customize displayed information.

;;; Code:

(require 'battery)
(require 'nerd-icons)

(unless (bound-and-true-p battery-status-function)
  (battery-update-handler))

(defcustom cocaine-battery-show-percentage t
  "When non-nil, display battery percentage as text after the icon."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-battery-info t
  "If set to t, show battery icons with nerdicons."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-battery-cache-update-interval 60
  "Interval in seconds for updating the Battery- cache."
  :type 'number
  :group 'cocaine-line)

(defvar cocaine-battery-info-cache nil
  "Cache for Battery information.")

(defvar cocaine-battery-info-cache-time 0
  "Time of last cache update.")

(defun cocaine-battery-create-info ()
  "Create battery percentage or charging status using text and nerd-font icons."
  (when (and cocaine-show-battery-info
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
           (percentage-text (if cocaine-battery-show-percentage
                                (format " %d%%" percentage)
                              "")))
      (if (and percentage status)
          (propertize
           (format "%s %s%%"
                   icon
                   percentage-text)
           'face face)
        "No battery info"))))

(defun cocaine-battery-info ()
  "Return battery information, updating the cache if necessary."
  (let ((current-time (float-time)))
    (when (or (null cocaine-battery-info-cache)
	      (> (- current-time cocaine-battery-info-cache-time) cocaine-battery-cache-update-interval))
      (setq cocaine-battery-info-cache-time current-time)
      (setq cocaine-battery-info-cache (cocaine-battery-create-info)))
      cocaine-battery-info-cache))

(provide 'cocaine-line-battery)
;;; cocaine-line-battery.el ends here 