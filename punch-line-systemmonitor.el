(defvar punch-cpu-usage nil
  "Stores the current CPU usage.")

(defvar punch-memory-usage nil
  "Stores the current memory usage.")

(defvar punch-update-interval 5
  "Number of seconds between updates of CPU and memory usage.")

(defvar punch-last-update 0
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
  (setq punch-last-update (float-time)))

(defun punch-system-monitor ()
  "Return a string with current CPU and memory usage."
  (when (> (- (float-time) punch-last-update) punch-update-interval)
    (punch-update-usage))
  (format "%s%% | %s GB"
          (if punch-cpu-usage (format "%.1f%%" (string-to-number punch-cpu-usage)) "N/A")
          (if punch-memory-usage (format "%.1f" (/ punch-memory-usage 1024)) "N/A")))

(provide 'punch-line-systemmonitor)
;;; punch-line-systemmonitor.el ends here
