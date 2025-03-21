;;; punch-line-weather.el --- A weather component for customized Emacs mode-line -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
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

(defvar punch-weather-temperature nil
  "Current temperature.")

(defvar punch-weather-icon nil
  "Current weather icon.")

(defvar punch-weather-description nil
  "Current weather description.")

(defun punch-weather--icon-from-code (code)
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

(defun punch-weather--fetch-data ()
  "Fetch weather data from API."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&current_weather=true"
                     punch-weather-latitude
                     punch-weather-longitude)))
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
                      (setq punch-weather-icon (punch-weather--icon-from-code weather-code))
                      (setq punch-weather-temperature
                            (if punch-weather-use-fahrenheit
                                (format "%.1f°F" (+ (* temp 1.8) 32))
                              (format "%.1f°C" temp)))
                      (setq punch-weather-description
                            (punch-weather--description-from-code weather-code))))
                  nil t)))

(defun punch-weather-info ()
  "Return formatted weather information for the mode-line."
  (when punch-show-weather-info
    (unless (and punch-weather-temperature punch-weather-icon)
      (punch-weather--fetch-data))
    (if (and punch-weather-temperature punch-weather-icon)
        (concat
         (propertize punch-weather-icon 'help-echo punch-weather-description)
         " "
         (propertize punch-weather-temperature 'face 'font-lock-constant-face
                     'help-echo punch-weather-description))
      "Loading...")))

(defun punch-weather-update ()
  "Update weather data periodically."
  (punch-weather--fetch-data)
  (run-with-timer punch-weather-update-interval nil 'punch-weather-update))

;; Start the update cycle

(provide 'punch-line-weather)
;;; punch-line-weather.el ends here
