;;; punch-line-weather.el --- A weather component for customized Emacs mode-line -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.1
;; Package-Requires: ((emacs "28.1") (nerd-icons "0.0.1"))

;;; Commentary:
;; This package provides a weather component for a customized mode-line in Emacs.
;; It fetches weather data from the Open-Meteo API and displays it using nerd-icons.

;;; Code:

(require 'url)
(require 'json)
(require 'nerd-icons)

(defgroup punch-weather nil
  "Customization group for punch-line-weather."
  :group 'punch-line)

(defcustom punch-show-weather-info t
  "If non-nil, display weather information in the mode-line."
  :type 'boolean
  :group 'punch-weather)

(defcustom punch-weather-latitude "0"
  "Latitude for weather data."
  :type 'string
  :group 'punch-weather)

(defcustom punch-weather-longitude "0"
  "Longitude for weather data."
  :type 'string
  :group 'punch-weather)

(defcustom punch-weather-use-fahrenheit nil
  "If non-nil, display temperature in Fahrenheit."
  :type 'boolean
  :group 'punch-weather)

(defcustom punch-weather-update-interval 3600
  "Interval in seconds for updating weather data."
  :type 'integer
  :group 'punch-weather)

(defcustom punch-weather-request-timeout 10
  "Timeout in seconds for weather API requests."
  :type 'integer
  :group 'punch-weather)

(defcustom punch-weather-max-retries 3
  "Maximum number of retry attempts for failed requests."
  :type 'integer
  :group 'punch-weather)

(defvar punch-weather-temperature nil
  "Current temperature.")

(defvar punch-weather-icon nil
  "Current weather icon.")

(defvar punch-weather-description nil
  "Current weather description.")

(defvar punch-weather--retry-count 0
  "Current retry count for failed requests.")

(defvar punch-weather--update-timer nil
  "Timer for periodic weather updates.")

(defvar punch-weather--request-buffer nil
  "Buffer used for the current weather request.")

(defvar punch-weather--fetching-p nil
  "Non-nil when a weather fetch is in progress.")

(defun punch-weather--icon-from-code (code)
  "Return a nerd-icon based on the weather CODE."
  (condition-case nil
      (cond
       ((member code '(0 1)) (nerd-icons-mdicon "nf-md-weather_sunny" :v-adjust 0.1))           ; Clear sky
       ((member code '(2 3)) (nerd-icons-mdicon "nf-md-weather_partly_cloudy" :v-adjust 0.1))   ; Partly cloudy
       ((member code '(45 48)) (nerd-icons-mdicon "nf-md-weather_fog" :v-adjust 0.1))           ; Fog
       ((member code '(51 53 55)) (nerd-icons-mdicon "nf-md-weather_rainy" :v-adjust 0.1))      ; Drizzle
       ((member code '(61 63 65)) (nerd-icons-mdicon "nf-md-weather_pouring" :v-adjust 0.1))    ; Rain
       ((member code '(71 73 75)) (nerd-icons-mdicon "nf-md-weather_snowy" :v-adjust 0.1))      ; Snow
       ((member code '(77)) (nerd-icons-mdicon "nf-md-weather_snowy_heavy" :v-adjust 0.1))      ; Snow grains
       ((member code '(80 81 82)) (nerd-icons-mdicon "nf-md-weather_pouring" :v-adjust 0.1))    ; Rain showers
       ((member code '(85 86)) (nerd-icons-mdicon "nf-md-weather_snowy_heavy" :v-adjust 0.1))   ; Snow showers
       ((member code '(95 96 99)) (nerd-icons-mdicon "nf-md-weather_lightning" :v-adjust 0.1))  ; Thunderstorm
       (t (nerd-icons-mdicon "nf-md-weather_cloudy" :v-adjust 0.1)))                           ; Default
    (error "?")))  ; Fallback to simple string if nerd-icons fails

(defun punch-weather--description-from-code (code)
  "Return a weather description based on the CODE."
  (cond
   ((member code '(0 1)) "Clear sky")
   ((member code '(2 3)) "Partly cloudy")
   ((member code '(45 48)) "Foggy")
   ((member code '(51 53 55)) "Drizzle")
   ((member code '(61 63 65)) "Rainy")
   ((member code '(71 73 75)) "Snowy")
   ((member code '(77)) "Snow grains")
   ((member code '(80 81 82)) "Rain showers")
   ((member code '(85 86)) "Snow showers")
   ((member code '(95 96 99)) "Thunderstorm")
   (t "Cloudy")))

(defun punch-weather--cleanup-old-buffers ()
  "Clean up old weather request buffers."
  (dolist (buffer (buffer-list))
    (let ((buf-name (buffer-name buffer)))
      (when (and buf-name (string-match "\\*http api.open-meteo.com" buf-name))
        (let ((proc (get-buffer-process buffer)))
          (when proc
            (set-process-query-on-exit-flag proc nil)  ; Don't prompt user
            (delete-process proc)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun punch-weather--handle-response (status)
  "Handle weather API response with STATUS."
  (let ((response-buffer (current-buffer)))
    (unwind-protect
        (condition-case err
            (progn
              ;; Only proceed if we're still in a live buffer
              (unless (buffer-live-p response-buffer)
                (error "Response buffer no longer exists"))

              ;; Check for errors in status
              (when (plist-get status :error)
                (error "Weather API request failed: %s" (plist-get status :error)))

              ;; Parse response
              (goto-char (point-min))
              (unless (re-search-forward "^$" nil t)
                (error "Invalid response format"))

              (let* ((json-object-type 'hash-table)
                     (json-array-type 'list)
                     (json-key-type 'symbol)
                     (json-data (json-read-from-string
                                (buffer-substring-no-properties (point) (point-max))))
                     (current-weather (gethash 'current_weather json-data)))

                (unless current-weather
                  (error "No weather data in response"))

                (let ((temp (gethash 'temperature current-weather))
                      (weather-code (gethash 'weathercode current-weather)))
                  (setq punch-weather-icon (punch-weather--icon-from-code weather-code))
                  (setq punch-weather-temperature
                        (if punch-weather-use-fahrenheit
                            (format "%.1f°F" (+ (* temp 1.8) 32))
                          (format "%.1f°C" temp)))
                  (setq punch-weather-description
                        (punch-weather--description-from-code weather-code))
                  (setq punch-weather--retry-count 0)
                  (setq punch-weather--fetching-p nil)
                  (message "Weather updated: %s %s" punch-weather-temperature punch-weather-description))))

          (error
           (setq punch-weather--fetching-p nil)
           (message "Weather fetch error: %s" (error-message-string err))
           (punch-weather--schedule-retry)))

      ;; Always cleanup buffer, but only if it still exists
      (when (and response-buffer (buffer-live-p response-buffer))
        (with-current-buffer response-buffer
          (let ((proc (get-buffer-process response-buffer)))
            (when (and proc (process-live-p proc))
              (set-process-query-on-exit-flag proc nil)
              (delete-process proc)))
          (kill-buffer response-buffer))))))

(defun punch-weather--schedule-retry ()
  "Schedule a retry with exponential backoff."
  (when (< punch-weather--retry-count punch-weather-max-retries)
    (setq punch-weather--retry-count (1+ punch-weather--retry-count))
    (let ((delay (* (expt 2 punch-weather--retry-count) 5)))
      (message "Retrying weather fetch in %d seconds (attempt %d/%d)..."
               delay punch-weather--retry-count punch-weather-max-retries)
      (run-with-timer delay nil #'punch-weather--fetch-data))))

(defun punch-weather--fetch-data ()
  "Fetch weather data from API with timeout and error handling."
  ;; Don't fetch if already fetching
  (unless punch-weather--fetching-p
    ;; Set fetching flag
    (setq punch-weather--fetching-p t)

    ;; Clean up any old hanging buffers first
    (punch-weather--cleanup-old-buffers)

    ;; Validate coordinates before making request
    (unless (and punch-weather-latitude punch-weather-longitude
                 (stringp punch-weather-latitude)
                 (stringp punch-weather-longitude))
      (setq punch-weather--fetching-p nil)
      (message "Weather: Invalid coordinates configured")
      (error "Invalid weather coordinates"))

    (let ((url-request-method "GET")
          (url-request-extra-headers '(("Content-Type" . "application/json")))
          (url (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&current_weather=true"
                       punch-weather-latitude
                       punch-weather-longitude)))

      (condition-case err
          (let ((buffer (url-retrieve url
                                     #'punch-weather--handle-response
                                     nil t)))
            (setq punch-weather--request-buffer buffer)

            ;; Disable exit query on the process to avoid user prompts
            (when buffer
              (let ((proc (get-buffer-process buffer)))
                (when proc
                  (set-process-query-on-exit-flag proc nil))))

            ;; Set up timeout
            (run-with-timer punch-weather-request-timeout nil
                           (lambda (buf)
                             (when (and buf (buffer-live-p buf))
                               (let ((proc (get-buffer-process buf)))
                                 (when (and proc (process-live-p proc))
                                   (set-process-query-on-exit-flag proc nil)
                                   (delete-process proc)
                                   (setq punch-weather--fetching-p nil)
                                   (message "Weather request timed out after %d seconds"
                                           punch-weather-request-timeout)
                                   (punch-weather--schedule-retry)))
                               (kill-buffer buf)))
                           buffer))

        (error
         (setq punch-weather--fetching-p nil)
         (message "Failed to start weather request: %s" (error-message-string err))
         (punch-weather--schedule-retry))))))

(defun punch-weather-info ()
  "Return formatted weather information for the mode-line."
  (when punch-show-weather-info
    (unless (and punch-weather-temperature punch-weather-icon)
      (punch-weather--fetch-data))
    (if (and punch-weather-temperature
             punch-weather-icon
             (stringp punch-weather-temperature)
             (stringp punch-weather-icon))
        (concat
         (propertize punch-weather-icon
                     'help-echo (or punch-weather-description "Weather"))
         " "
         (propertize punch-weather-temperature
                     'face 'font-lock-constant-face
                     'help-echo (or punch-weather-description "Weather")))
      "Loading...")))

(defun punch-weather-update ()
  "Update weather data periodically."
  (punch-weather--fetch-data)
  (when punch-weather--update-timer
    (cancel-timer punch-weather--update-timer))
  (setq punch-weather--update-timer
        (run-with-timer punch-weather-update-interval nil #'punch-weather-update)))

(defun punch-weather-cleanup ()
  "Clean up all weather-related timers and buffers."
  (interactive)
  (when punch-weather--update-timer
    (cancel-timer punch-weather--update-timer)
    (setq punch-weather--update-timer nil))
  (punch-weather--cleanup-old-buffers)
  (setq punch-weather--retry-count 0)
  (setq punch-weather--fetching-p nil)
  (message "Weather cleanup complete"))

(defun punch-weather-force-update ()
  "Force an immediate weather update."
  (interactive)
  (setq punch-weather--retry-count 0)
  (setq punch-weather--fetching-p nil)
  (punch-weather--cleanup-old-buffers)
  (punch-weather--fetch-data))

;; Note: The update cycle should be started manually via `punch-weather-update`
;; or through a hook (e.g., after-init-hook) to avoid duplicate initialization.

(provide 'punch-line-weather)
;;; punch-line-weather.el ends here
