;;; cocaine-line-music.el --- Customized mode-line with music info -*- lexical-binding: t; -*-
;; Author: Mikael Konradsson
;; Version: 2.0
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; This package provides a customized mode-line component for displaying music information.

;;; Code:

(require 'subr-x)  ; for string-trim
(require 'nerd-icons)

(defgroup cocaine-line nil
  "Customization group for cocaine-line."
  :group 'mode-line)

(defcustom cocaine-line-music-info nil
  "Configuration for music information display in the mode-line.
If nil, music information is not displayed.
If t, music information is displayed with default settings.
If a plist, it can contain the following properties:
  :service - Symbol 'apple or 'spotify to specify the music service."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Enabled (default)" t)
                 (plist :tag "Custom configuration"
                        :options ((:service (choice (const apple) (const spotify))))))
  :group 'cocaine-line)

(defvar cocaine-music-info-cache "No music playing"
  "Cache for music information.")

(defvar cocaine-music-info-last-update 0
  "Timestamp of the last music info update.")

(defvar cocaine-music-info-update-interval 10
  "Interval in seconds between music info updates.")

(defvar cocaine-music-info-timer nil
  "Timer for updating music information.")

(defun cocaine-line-music-info-command (service)
  "Return the AppleScript command to get music info for SERVICE ('apple or 'spotify)."
  (let ((app-name (if (eq service 'apple) "Music" "Spotify")))
    (format "
tell application \"System Events\"
  if exists process \"%s\" then
    tell application \"%s\"
      if player state is playing then
        set track_name to name of current track
        set artist_name to artist of current track
        return track_name & \"|\" & artist_name
      else
        return \"No music playing\"
      end if
    end tell
  else
    return \"Application not running\"
  end if
end tell" app-name app-name)))

(defun cocaine-line-get-music-service ()
  "Get the configured music service or default to 'apple."
  (cond
   ((null cocaine-line-music-info) nil)
   ((eq cocaine-line-music-info t) 'apple)
   ((and (plist-member cocaine-line-music-info :service)
         (memq (plist-get cocaine-line-music-info :service) '(apple spotify)))
    (plist-get cocaine-line-music-info :service))
   (t 'apple)))

(defun cocaine-line-update-music-info-async ()
  "Update the cached music info asynchronously."
  (let ((current-time (float-time))
        (service (cocaine-line-get-music-service)))
    (when (and service
               (or (null cocaine-music-info-cache)
                   (> (- current-time cocaine-music-info-last-update)
                      cocaine-music-info-update-interval)))
      (setq cocaine-music-info-last-update current-time)
      (let* ((buffer-name "*cocaine-line-music-info*")
             (process
              (start-process "cocaine-line-music-info"
                             buffer-name
                             "osascript"
                             "-e" (cocaine-line-music-info-command service))))
        (set-process-sentinel
         process
         (lambda (proc event)
           (when (string= event "finished\n")
             (let ((output (with-current-buffer (process-buffer proc)
                             (buffer-string))))
               (setq cocaine-music-info-cache
                     (when (not (string-empty-p (string-trim output)))
                       (concat " " (cocaine-line-icon) " " (string-trim output))))
               (force-mode-line-update t)))
           ;; Clean up process buffer after we're done with it
           (kill-buffer (process-buffer proc))))))))

(defun cocaine-line-icon ()
  "Return the icon for the music service."
  (let ((service (cocaine-line-get-music-service)))
    (cond
     ((eq service 'apple) (nerd-icons-faicon "nf-fa-music" :v-adjust 0.1))
     ((eq service 'spotify) (nerd-icons-faicon "nf-fa-spotify" :v-adjust 0.0))
     (t ))))

(defun cocaine-line-start-music-info-timer ()
  "Start the timer for updating music information."
  (when (and cocaine-line-music-info (null cocaine-music-info-timer))
    (setq cocaine-music-info-timer
          (run-at-time 0 cocaine-music-info-update-interval #'cocaine-line-update-music-info-async))
    (message "Started cocaine-line music info timer")))

(defun cocaine-line-stop-music-info-timer ()
  "Stop the timer for updating music information."
  (when cocaine-music-info-timer
    (cancel-timer cocaine-music-info-timer)
    (setq cocaine-music-info-timer nil)
    (message "Stopped cocaine-line music info timer")))

(defun cocaine-line-get-music-info ()
  "Get the current cached music info."
  cocaine-music-info-cache)

(defun cocaine-line-music-info ()
  "Return the current music info for display in the mode-line."
  (when cocaine-line-music-info
    (cocaine-line-get-music-info)))

(add-hook 'emacs-startup-hook #'cocaine-line-start-music-info-timer)
(add-hook 'kill-emacs-hook #'cocaine-line-stop-music-info-timer)

(provide 'cocaine-line-music)
;;; cocaine-line-music.el ends here
