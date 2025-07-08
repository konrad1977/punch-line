;;; cocaine-line-weather.el --- A weather component for customized Emacs mode-line -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (nerd-icons "0.0.1"))

;;; Commentary:
;; This package provides a weather component for a customized mode-line in Emacs.
;; It fetches weather data from the Open-Meteo API and displays it using nerd-icons.

;;; Code:

(require 'url)
(require 'json)
(require 'nerd-icons)

(defcustom cocaine-show-weather-info t
  "If non-nil, display weather information in the mode-line."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-weather-latitude "0"
  "Latitude for weather data."
  :type 'string
  :group 'cocaine-line)

(defcustom cocaine-weather-longitude "0"
  "Longitude for weather data."
  :type 'string
  :group 'cocaine-line)

(defcustom cocaine-weather-use-fahrenheit nil
  "If non-nil, display temperature in Fahrenheit."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-weather-update-interval 3600
  "Interval in seconds for updating weather data."
  :type 'integer
  :group 'cocaine-line)

(defvar cocaine-weather-temperature nil
  "Current temperature.")

(defvar cocaine-weather-icon nil
  "Current weather icon.")

(defvar cocaine-weather-description nil
  "Current weather description.")

(defun cocaine-weather--icon-from-code (code)
  "Return a nerd-icon based on the weather CODE."
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
   (t (nerd-icons-mdicon "nf-md-weather_cloudy" :v-adjust 0.1))))                           ; Default

(defun cocaine-weather--description-from-code (code)
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

(defun cocaine-weather--fetch-data ()
  "Fetch weather data from API."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&current_weather=true"
                     cocaine-weather-latitude
                     cocaine-weather-longitude)))
    (url-retrieve url
                  (lambda (_)
                    (goto-char (point-min))
                    (re-search-forward "^$")
                    (let* ((json-object-type 'hash-table)
                           (json-array-type 'list)
                           (json-key-type 'symbol)
                           (json-data (json-read-from-string
                                       (buffer-substring-no-properties (point) (point-max))))
                           (current-weather (gethash 'current_weather json-data))
                           (temp (gethash 'temperature current-weather))
                           (weather-code (gethash 'weathercode current-weather)))
                      (setq cocaine-weather-icon (cocaine-weather--icon-from-code weather-code))
                      (setq cocaine-weather-temperature
                            (if cocaine-weather-use-fahrenheit
                                (format "%.1f°F" (+ (* temp 1.8) 32))
                              (format "%.1f°C" temp)))
                      (setq cocaine-weather-description
                            (cocaine-weather--description-from-code weather-code))))
                  nil t)))

(defun cocaine-weather-info ()
  "Return formatted weather information for the mode-line."
  (when cocaine-show-weather-info
    (unless (and cocaine-weather-temperature cocaine-weather-icon)
      (cocaine-weather--fetch-data))
    (if (and cocaine-weather-temperature cocaine-weather-icon)
        (concat
         (propertize cocaine-weather-icon 'help-echo cocaine-weather-description)
         " "
         (propertize cocaine-weather-temperature 'face 'font-lock-constant-face
                     'help-echo cocaine-weather-description))
      "Loading...")))

(defun cocaine-weather-update ()
  "Update weather data periodically."
  (cocaine-weather--fetch-data)
  (run-with-timer cocaine-weather-update-interval nil 'cocaine-weather-update))

;; Start the update cycle

(provide 'cocaine-line-weather)
;;; cocaine-line-weather.el ends here 