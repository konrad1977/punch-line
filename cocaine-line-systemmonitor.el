;;; cocaine-line-systemmonitor.el --- A customized mode-line for Emacs with modal status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs with modal status,
;; configurable colors, and the ability to customize displayed information.

;;; Code:

(require 'nerd-icons)
(require 'cocaine-line-colors)

(defcustom cocaine-show-system-monitor nil
  "Whether to show system monitor information in the mode-line."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-system-monitor-use-icons t
  "Whether to use icons for system monitor information."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-system-monitor-update-interval 5
  "Interval in seconds for updating the system monitor cache."
  :type 'number
  :group 'cocaine-line)

(defvar cocaine-system-monitor-cache nil
  "Stores the current system monitor information.")

(defvar cocaine-cpu-usage nil
  "Stores the current CPU usage.")

(defvar cocaine-memory-usage nil
  "Stores the current memory usage.")

(defvar cocaine-system-monitor-last-update 0
  "Time of last update.")

(defun cocaine-get-cpu-usage ()
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

(defun cocaine-get-memory-usage ()
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

(defun cocaine-system-monitor-create ()
  "Update CPU and memory usage."
  (when cocaine-show-system-monitor
    (setq cocaine-cpu-usage (cocaine-get-cpu-usage))
    (setq cocaine-memory-usage (cocaine-get-memory-usage))
    (setq cocaine-system-monitor-last-update (float-time))
    (format "%s %s  %s %s"
            (if cocaine-system-monitor-use-icons
                (nerd-icons-octicon "nf-oct-cpu")
              "CPU:")
            (propertize
             (if cocaine-cpu-usage (format "%.1f%%" (string-to-number cocaine-cpu-usage)) "N/A")
             'face 'cocaine-line-system-monitor-cpu-face)
            (if cocaine-system-monitor-use-icons
                (concat (nerd-icons-faicon "nf-fa-memory") " ")
              "MEM:")
            (propertize
             (if cocaine-memory-usage (format "%.1f GB" (/ cocaine-memory-usage 1024)) "N/A")
             'face 'cocaine-line-system-monitor-memory-face))))

(defun cocaine-system-monitor-info ()
  "Return a string with current CPU and memory usage."
  (let ((current-time (float-time)))
    (when (or (null cocaine-system-monitor-cache)
              (> (- current-time cocaine-system-monitor-last-update) cocaine-system-monitor-update-interval))
      (setq cocaine-system-monitor-last-update current-time)
      (setq cocaine-system-monitor-cache (cocaine-system-monitor-create)))
      cocaine-system-monitor-cache))

(provide 'cocaine-line-systemmonitor)
;;; cocaine-line-systemmonitor.el ends here 