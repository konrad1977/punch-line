;;; punch-line-systemonitor.el --- A customized mode-line for Emacs with modal status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs with modal status,
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

(defcustom punch-system-monitor-update-interval 5
  "Interval in seconds for updating the system monitor cache."
  :type 'number
  :group 'punch-line)

(defvar punch-system-monitor-cache nil
  "Stores the current system monitor information.")

(defvar punch-cpu-usage nil
  "Stores the current CPU usage.")

(defvar punch-memory-usage nil
  "Stores the current memory usage.")

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

(defun punch-system-monitor-create ()
  "Update CPU and memory usage."
  (when punch-show-system-monitor
    (setq punch-cpu-usage (punch-get-cpu-usage))
    (setq punch-memory-usage (punch-get-memory-usage))
    (setq punch-system-monitor-last-update (float-time))
    (format "%s %s  %s %s"
            (if punch-system-monitor-use-icons
                (nerd-icons-octicon "nf-oct-cpu")
              "CPU:")
            (propertize
             (if punch-cpu-usage (format "%.1f%%" (string-to-number punch-cpu-usage)) "N/A")
             'face 'punch-line-system-monitor-cpu-face)
            (if punch-system-monitor-use-icons
                (concat (nerd-icons-faicon "nf-fa-memory") " ")
              "MEM:")
            (propertize
             (if punch-memory-usage (format "%.1f GB" (/ punch-memory-usage 1024)) "N/A")
             'face 'punch-line-system-monitor-memory-face))))

(defun punch-system-monitor-info ()
  "Return a string with current CPU and memory usage."
  (let ((current-time (float-time)))
    (when (or (null punch-system-monitor-cache)
              (> (- current-time punch-system-monitor-last-update) punch-system-monitor-update-interval))
      (setq punch-system-monitor-last-update current-time)
      (setq punch-system-monitor-cache (punch-system-monitor-create)))
      punch-system-monitor-cache))

(provide 'punch-line-systemmonitor)
;;; punch-line-systemmonitor.el ends here
