;;; cocaine-line-music.el --- Customized mode-line with music info -*- lexical-binding: t; -*-
;; Author: Mikael Konradsson
;; Version: 2.0
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; This package provides a customized mode-line component for displaying music information.

;;; Code:

(require 'subr-x)  ; for string-trim
(require 'nerd-icons)

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

(defcustom cocaine-line-music-max-length 40
  "Maximum length of the music info displayed in the mode-line."
  :type 'number
  :group 'cocaine-line)

(defvar cocaine-music-info-cache ""
  "Cache for music information.")

(defvar cocaine-music-info-last-update 0
  "Timestamp of the last music info update.")

(defvar cocaine-music-info-update-interval 10
  "Interval in seconds between music info updates.")

(defvar cocaine-music-info-timer nil
  "Timer for updating music information.")

(defvar cocaine-line-process-timeout 5
  "Timeout in seconds for music info process.")

(defun cocaine-line-cleanup-stale-process ()
  "Clean up any stale music info process and its buffer."
  (let* ((buffer-name "*cocaine-line-music-info*")
         (buffer (get-buffer buffer-name))
         (process (and buffer (get-buffer-process buffer))))
    (when process
      (when (eq (process-status process) 'run)
        (delete-process process))
      (when buffer
        (kill-buffer buffer)))))

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
        return track_name & \" • \" & artist_name
      else
        return \"\"
      end if
    end tell
  else
    return \"\"
  end if
end tell" app-name app-name)))

(defun cocaine-line-get-music-service ()
  "Get the configured music service or default to apple."
  (cond
   ((null cocaine-line-music-info) nil)
   ((eq cocaine-line-music-info t) 'apple)
   ((and (plist-member cocaine-line-music-info :service)
         (memq (plist-get cocaine-line-music-info :service) '(apple spotify)))
    (plist-get cocaine-line-music-info :service))
   (t 'apple)))

(defun cocaine-line-update-music-info-async ()
  "Update the cached music info asynchronously with timeout handling."
  (let ((current-time (float-time))
        (service (cocaine-line-get-music-service)))
    (when (and service
               (or (null cocaine-music-info-cache)
                   (> (- current-time cocaine-music-info-last-update)
                      cocaine-music-info-update-interval)))
      (setq cocaine-music-info-last-update current-time)
      ;; Cleanup any existing process first
      (cocaine-line-cleanup-stale-process)
      (let* ((buffer-name "*cocaine-line-music-info*")
             (process
              (start-process "cocaine-line-music-info"
                           buffer-name
                           "osascript"
                           "-e" (cocaine-line-music-info-command service))))
        ;; Set process timeout
        (run-with-timer cocaine-line-process-timeout nil
                       (lambda ()
                         (when (and process (eq (process-status process) 'run))
                           (cocaine-line-cleanup-stale-process))))
        (set-process-sentinel
         process
         (lambda (proc event)
           (when (string= event "finished\n")
             (let ((output (with-current-buffer (process-buffer proc)
                            (buffer-string))))
               (setq cocaine-music-info-cache
                     (if (string-empty-p (string-trim output))
                         ""
                       (if (> cocaine-line-music-max-length 0)
                           (concat " " (cocaine-line-icon) " "
                                   (propertize (cocaine-line-trim-music-info output)
                                             'face 'cocaine-line-music-face))
                         (concat (cocaine-line-icon) " "))))
               (force-mode-line-update t)))
           ;; Clean up process buffer
           (when (buffer-live-p (process-buffer proc))
             (kill-buffer (process-buffer proc)))))))))

(defun cocaine-line-trim-music-info (info)
  "Trim the music INFO to a maximum length."
  (let ((max-length cocaine-line-music-max-length)
        (text (replace-regexp-in-string "\n" "" (string-trim info))))
    (if (> (length text) max-length)
        (concat (substring text 0 max-length) "...")
      text)))

(defun cocaine-line-icon ()
  "Return the icon for the music service."
  (let ((service (cocaine-line-get-music-service)))
    (cond
     ((eq service 'apple) (propertize (nerd-icons-faicon "nf-fa-music") 'face 'cocaine-line-music-apple-face))
     ((eq service 'spotify) (propertize (nerd-icons-faicon "nf-fa-spotify") 'face 'cocaine-line-music-spotify-face))
     (t ))))

(defun cocaine-line-start-music-info-timer ()
  "Start the timer for updating music information."
  (when (and cocaine-line-music-info (null cocaine-music-info-timer))
    (setq cocaine-music-info-timer
          (run-at-time 0 cocaine-music-info-update-interval #'cocaine-line-update-music-info-async))))

(defun cocaine-line-stop-music-info-timer ()
  "Stop the timer and clean up any running process."
  (when cocaine-music-info-timer
    (cancel-timer cocaine-music-info-timer)
    (setq cocaine-music-info-timer nil))
  (cocaine-line-cleanup-stale-process))

(defun cocaine-line-get-music-info ()
  "Get the current cached music info with a limited length."
  cocaine-music-info-cache)

(defun cocaine-line-music-info ()
  "Return the current music info for display in the mode-line."
  (when (and cocaine-line-music-info (cocaine-line-get-music-service))
    (cocaine-line-get-music-info)))

(when (eq system-type 'darwin)
  (add-hook 'emacs-startup-hook #'cocaine-line-start-music-info-timer)
  (add-hook 'kill-emacs-hook #'cocaine-line-stop-music-info-timer))

(provide 'cocaine-line-music)
;;; cocaine-line-music.el ends here 