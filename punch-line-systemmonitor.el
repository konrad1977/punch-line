;;; punch-line-systemonitor.el --- A customized mode-line for Emacs with Evil status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (evil "1.0.0"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs with Evil status,
;; configurable colors, and the ability to customize displayed information.

;;; Code:

(require 'nerd-icons)
(require 'punch-line-colors)

(defcustom punch-show-system-monitor nil
  "Whether to show system monitor information in the mode-line."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-system-monitor-use-icons t
  "Whether to use icons for system monitor information."
  :type 'boolean
  :group 'punch-line)

(defvar punch-cpu-usage nil
  "Stores the current CPU usage.")

(defvar punch-memory-usage nil
  "Stores the current memory usage.")

(defvar punch-system-monitor-update-interval 5
  "Number of seconds between updates of CPU and memory usage.")

(defvar punch-system-monitor-last-update 0
  "Time of last update.")

(defun punch-get-cpu-usage ()
  "Get the current CPU usage."
  (if (eq system-type 'darwin)
      ;; macOS
      (with-temp-buffer
        (call-process "sh" nil t nil "-c"
                      "top -l 1 | grep -E \"^CPU\" | awk '{print $3 + $5}'")
        (string-trim (buffer-string)))
    ;; Linux
    (with-temp-buffer
      (call-process "sh" nil t nil "-c"
                    "top -bn1 | grep \"Cpu(s)\" | sed \"s/.*, *\\([0-9.]*\\)%* id.*/\\1/\" | awk '{print 100 - $1}'")
      (string-trim (buffer-string)))))

(defun punch-get-memory-usage ()
  "Get the current memory usage in MB."
  (with-temp-buffer
    (if (eq system-type 'darwin)
        ;; macOS
        (call-process "sh" nil t nil "-c"
                      "vm_stat | awk '/Pages active/ {print $3}' | sed 's/\\.//' | awk '{printf \"%.0f\", $1 * 4 / 1024}'")
      ;; Linux
      (call-process "sh" nil t nil "-c"
                    "free -m | awk '/^Mem:/ {print $3}'"))
    (string-to-number (buffer-string))))

(defun punch-update-usage ()
  "Update CPU and memory usage."
  (setq punch-cpu-usage (punch-get-cpu-usage))
  (setq punch-memory-usage (punch-get-memory-usage))
  (setq punch-system-monitor-last-update (float-time)))


(defun punch-system-monitor-info ()
  "Return a string with current CPU and memory usage."
  (if punch-show-system-monitor
      (progn
        (when (> (- (float-time) punch-system-monitor-last-update) punch-system-monitor-update-interval)
          (punch-update-usage))
        (format "%s %s %s %s"
                (if punch-system-monitor-use-icons
                    (nerd-icons-octicon "nf-oct-cpu")
                  "CPU:")
                (propertize
                 (if punch-cpu-usage (concat (format "%.1f" (string-to-number punch-cpu-usage)) "%%") "N/A")
                 'face 'punch-line-system-monitor-cpu-face)
                (if punch-system-monitor-use-icons
                    (concat (nerd-icons-faicon "nf-fa-memory") " ")
                  "MEM:")
                (propertize
                 (if punch-memory-usage (format "%.1fGB" (/ punch-memory-usage 1024)) "N/A")
                 'face 'punch-line-system-monitor-memory-face)))
    ""))

(provide 'punch-line-systemmonitor)
;;; punch-line-systemmonitor.el ends here
