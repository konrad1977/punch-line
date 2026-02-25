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

(defvar punch-weather--last-successful-fetch nil
  "Timestamp of last successful weather fetch.")

(defvar punch-weather--last-fetch-attempt nil
  "Timestamp of last fetch attempt (successful or not).")

(defvar punch-weather--consecutive-failures 0
  "Count of consecutive failures for backoff calculation.")

(defvar punch-weather--paused-p nil
  "Non-nil when weather polling is paused (e.g., Emacs lost focus).")

(defvar punch-weather--retry-timer nil
  "Timer for retry attempts.")

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

              (let ((json-string (buffer-substring-no-properties (point) (point-max))))
                ;; Validate we have actual JSON content
                (when (or (string-empty-p (string-trim json-string))
                          (< (length json-string) 10))
                  (error "Empty or invalid JSON response"))

                (let* ((json-object-type 'hash-table)
                       (json-array-type 'list)
                       (json-key-type 'symbol)
                       (json-data (condition-case json-err
                                      (json-read-from-string json-string)
                                    (error
                                     (error "JSON parsing failed: %s" (error-message-string json-err)))))
                       (current-weather (gethash 'current_weather json-data)))

                  (unless current-weather
                    (error "No weather data in response"))

                  (let ((temp (gethash 'temperature current-weather))
                        (weather-code (gethash 'weathercode current-weather)))
                    (unless (and temp weather-code)
                      (error "Invalid weather data structure"))
                    (setq punch-weather-icon (punch-weather--icon-from-code weather-code))
                    (setq punch-weather-temperature
                          (if punch-weather-use-fahrenheit
                              (format "%.1f°F" (+ (* temp 1.8) 32))
                            (format "%.1f°C" temp)))
                    (setq punch-weather-description
                          (punch-weather--description-from-code weather-code))
                    (setq punch-weather--retry-count 0)
                    (setq punch-weather--consecutive-failures 0)
                    (setq punch-weather--last-successful-fetch (current-time))
                    (setq punch-weather--fetching-p nil)
                    ;; Only show update message if we had previous failures
                    (when (> punch-weather--consecutive-failures 0)
                      (message "Weather updated: %s %s" punch-weather-temperature punch-weather-description))))))

          (error
           (setq punch-weather--fetching-p nil)
           (setq punch-weather--consecutive-failures (1+ punch-weather--consecutive-failures))
           ;; Only log errors sparingly - first failure and then every 10th
           (when (or (= punch-weather--consecutive-failures 1)
                     (= 0 (% punch-weather--consecutive-failures 10)))
             (message "Weather fetch error: %s (attempt %d)"
                      (error-message-string err)
                      punch-weather--consecutive-failures))
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
  (when (and (< punch-weather--retry-count punch-weather-max-retries)
             (not punch-weather--paused-p))
    (setq punch-weather--retry-count (1+ punch-weather--retry-count))
    ;; Use longer backoff times, especially for consecutive failures
    (let ((delay (* (expt 2 (+ punch-weather--retry-count
                                (min 3 (/ punch-weather--consecutive-failures 2))))
                    10)))
      ;; Only show retry message on first attempt
      (when (= punch-weather--retry-count 1)
        (message "Retrying weather fetch in %d seconds..."
                 delay))
      ;; Cancel any existing retry timer before scheduling a new one
      (when (timerp punch-weather--retry-timer)
        (cancel-timer punch-weather--retry-timer))
      (setq punch-weather--retry-timer
            (run-with-timer delay nil #'punch-weather--fetch-data)))))

(defun punch-weather--fetch-data ()
  "Fetch weather data from API with timeout and error handling."
  ;; Don't fetch if already fetching or if paused
  (unless (or punch-weather--fetching-p punch-weather--paused-p)
    ;; Set fetching flag and record attempt time
    (setq punch-weather--fetching-p t)
    (setq punch-weather--last-fetch-attempt (current-time))

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
                                   (setq punch-weather--consecutive-failures
                                         (1+ punch-weather--consecutive-failures))
                                    ;; Only log timeout sparingly
                                    (when (or (= punch-weather--consecutive-failures 1)
                                              (= 0 (% punch-weather--consecutive-failures 10)))
                                      (message "Weather request timed out after %d seconds (attempt %d)"
                                              punch-weather-request-timeout
                                              punch-weather--consecutive-failures))
                                   (punch-weather--schedule-retry)))
                               (kill-buffer buf)))
                           buffer))

        (error
         (setq punch-weather--fetching-p nil)
         (setq punch-weather--consecutive-failures (1+ punch-weather--consecutive-failures))
         ;; Only log sparingly
         (when (or (= punch-weather--consecutive-failures 1)
                   (= 0 (% punch-weather--consecutive-failures 10)))
           (message "Failed to start weather request: %s (attempt %d)"
                    (error-message-string err)
                    punch-weather--consecutive-failures))
         (punch-weather--schedule-retry))))))

(defun punch-weather--should-fetch-p ()
  "Return non-nil if enough time has passed to attempt a new fetch.
Uses exponential backoff based on consecutive failures."
  (or (null punch-weather--last-fetch-attempt)
      (let* ((backoff-seconds (if (> punch-weather--consecutive-failures 0)
                                  ;; Exponential backoff: 30s, 60s, 120s, 240s... capped at 30min
                                  (min 1800 (* 30 (expt 2 (1- punch-weather--consecutive-failures))))
                                10))
             (elapsed (float-time (time-subtract (current-time)
                                                 punch-weather--last-fetch-attempt))))
        (> elapsed backoff-seconds))))

(defun punch-weather-info ()
  "Return formatted weather information for the mode-line."
  (when punch-show-weather-info
    (when (and (not (and punch-weather-temperature punch-weather-icon))
               (not punch-weather--fetching-p)
               (punch-weather--should-fetch-p))
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
      "")))

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
  (when (timerp punch-weather--update-timer)
    (cancel-timer punch-weather--update-timer)
    (setq punch-weather--update-timer nil))
  (when (timerp punch-weather--retry-timer)
    (cancel-timer punch-weather--retry-timer)
    (setq punch-weather--retry-timer nil))
  (punch-weather--cleanup-old-buffers)
  (setq punch-weather--retry-count 0)
  (setq punch-weather--consecutive-failures 0)
  (setq punch-weather--fetching-p nil)
  (setq punch-weather--paused-p nil)
  (setq punch-weather--last-fetch-attempt nil)
  (message "Weather cleanup complete"))

(defun punch-weather-force-update ()
  "Force an immediate weather update."
  (interactive)
  (setq punch-weather--retry-count 0)
  (setq punch-weather--consecutive-failures 0)
  (setq punch-weather--fetching-p nil)
  (setq punch-weather--last-fetch-attempt nil)
  (punch-weather--cleanup-old-buffers)
  (punch-weather--fetch-data))

(defun punch-weather--on-focus-out ()
  "Pause weather polling when Emacs loses focus.
Cancels all pending timers and kills hanging HTTP processes."
  (setq punch-weather--paused-p t)
  ;; Cancel pending timers to prevent fetches while unfocused
  (when (timerp punch-weather--update-timer)
    (cancel-timer punch-weather--update-timer)
    (setq punch-weather--update-timer nil))
  (when (timerp punch-weather--retry-timer)
    (cancel-timer punch-weather--retry-timer)
    (setq punch-weather--retry-timer nil))
  ;; Kill any in-flight requests
  (punch-weather--cleanup-old-buffers)
  (setq punch-weather--fetching-p nil))

(defun punch-weather--on-focus-in ()
  "Resume weather polling when Emacs regains focus.
Cleans up stale buffers/processes and schedules a fresh update."
  (setq punch-weather--paused-p nil)
  ;; Always clean up any stale buffers/processes from the unfocused period
  (punch-weather--cleanup-old-buffers)
  (setq punch-weather--fetching-p nil)
  ;; Reset retry state so we get a fresh start
  (setq punch-weather--retry-count 0)
  ;; If enough time has passed since last successful fetch, update now
  (when (or (null punch-weather--last-successful-fetch)
            (> (float-time (time-subtract (current-time)
                                          punch-weather--last-successful-fetch))
               punch-weather-update-interval))
    (punch-weather--fetch-data))
  ;; Restart the periodic update timer
  (unless punch-weather--update-timer
    (setq punch-weather--update-timer
          (run-with-timer punch-weather-update-interval nil #'punch-weather-update))))

(defun punch-weather-enable-focus-hooks ()
  "Enable focus-based weather polling management."
  (add-hook 'focus-out-hook #'punch-weather--on-focus-out)
  (add-hook 'focus-in-hook #'punch-weather--on-focus-in))

(defun punch-weather-disable-focus-hooks ()
  "Disable focus-based weather polling management."
  (remove-hook 'focus-out-hook #'punch-weather--on-focus-out)
  (remove-hook 'focus-in-hook #'punch-weather--on-focus-in))

;; Automatically enable focus hooks when this module is loaded
(punch-weather-enable-focus-hooks)

;; Note: The update cycle should be started manually via `punch-weather-update`
;; or through a hook (e.g., after-init-hook) to avoid duplicate initialization.

(provide 'punch-line-weather)
;;; punch-line-weather.el ends here
