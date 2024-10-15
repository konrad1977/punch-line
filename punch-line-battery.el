;;; punch-line-battery.el --- A customized mode-line for Emacs with Evil status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (evil "1.0.0"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs with Evil status,
;; configurable colors, and the ability to customize displayed information.

;;; Code:

(require 'battery)
(require 'nerd-icons)

(unless (bound-and-true-p battery-status-function)
  (battery-update-handler))

(defcustom punch-battery-show-percentage t
  "When non-nil, display battery percentage as text after the icon."
  :type 'boolean
  :group 'punch)

(defcustom punch-show-battery-info t
  "If set to t, show battery icons with nerdicons."
  :type 'boolean
  :group 'punch-line)

(defun punch-battery-info ()
  "Show battery percentage or charging status using text and nerd-font icons on macOS with face-based coloring."
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

(provide 'punch-line-battery)
;;; punch-line-battery.el ends here