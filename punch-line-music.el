;;; punch-line-music.el --- Customized mode-line with music info -*- lexical-binding: t; -*-
;; Author: Mikael Konradsson
;; Version: 2.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; This package provides a customized mode-line component for displaying music information.

;;; Code:

(require 'subr-x)  ; for string-trim
(require 'nerd-icons)

(defgroup punch-line nil
  "Customization group for punch-line."
  :group 'mode-line)

(defcustom punch-line-music-info nil
  "Configuration for music information display in the mode-line.
If nil, music information is not displayed.
If t, music information is displayed with default settings.
If a plist, it can contain the following properties:
  :service - Symbol 'apple or 'spotify to specify the music service."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Enabled (default)" t)
                 (plist :tag "Custom configuration"
                        :options ((:service (choice (const apple) (const spotify))))))
  :group 'punch-line)

(defcustom punch-line-music-max-length 40
  "Maximum length of the music info displayed in the mode-line."
  :type 'number
  :group 'punch-line)

(defvar punch-music-info-cache ""
  "Cache for music information.")

(defvar punch-music-info-last-update 0
  "Timestamp of the last music info update.")

(defvar punch-music-info-update-interval 10
  "Interval in seconds between music info updates.")

(defvar punch-music-info-timer nil
  "Timer for updating music information.")

(defvar punch-line-process-timeout 5
  "Timeout in seconds for music info process.")

(defun punch-line-cleanup-stale-process ()
  "Clean up any stale music info process and its buffer."
  (let* ((buffer-name "*punch-line-music-info*")
         (buffer (get-buffer buffer-name))
         (process (and buffer (get-buffer-process buffer))))
    (when process
      (when (eq (process-status process) 'run)
        (delete-process process))
      (when buffer
        (kill-buffer buffer)))))

(defun punch-line-music-info-command (service)
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

(defun punch-line-get-music-service ()
  "Get the configured music service or default to apple."
  (cond
   ((null punch-line-music-info) nil)
   ((eq punch-line-music-info t) 'apple)
   ((and (plist-member punch-line-music-info :service)
         (memq (plist-get punch-line-music-info :service) '(apple spotify)))
    (plist-get punch-line-music-info :service))
   (t 'apple)))

(defun punch-line-update-music-info-async ()
  "Update the cached music info asynchronously with timeout handling."
  (let ((current-time (float-time))
        (service (punch-line-get-music-service)))
    (when (and service
               (or (null punch-music-info-cache)
                   (> (- current-time punch-music-info-last-update)
                      punch-music-info-update-interval)))
      (setq punch-music-info-last-update current-time)
      ;; Cleanup any existing process first
      (punch-line-cleanup-stale-process)
      (let* ((buffer-name "*punch-line-music-info*")
             (process
              (start-process "punch-line-music-info"
                           buffer-name
                           "osascript"
                           "-e" (punch-line-music-info-command service))))
        ;; Set process timeout
        (run-with-timer punch-line-process-timeout nil
                       (lambda ()
                         (when (and process (eq (process-status process) 'run))
                           (punch-line-cleanup-stale-process))))
        (set-process-sentinel
         process
         (lambda (proc event)
           (when (string= event "finished\n")
             (let ((output (with-current-buffer (process-buffer proc)
                            (buffer-string))))
               (setq punch-music-info-cache
                     (if (string-empty-p (string-trim output))
                         ""
                       (if (> punch-line-music-max-length 0)
                           (concat " " (punch-line-icon) " "
                                   (propertize (punch-line-trim-music-info output)
                                             'face 'punch-line-music-face))
                         (concat (punch-line-icon) " "))))
               (force-mode-line-update t)))
           ;; Clean up process buffer
           (when (buffer-live-p (process-buffer proc))
             (kill-buffer (process-buffer proc)))))))))

(defun punch-line-trim-music-info (info)
  "Trim the music INFO to a maximum length."
  (let ((max-length punch-line-music-max-length)
        (text (replace-regexp-in-string "\n" "" (string-trim info))))
    (if (> (length text) max-length)
        (concat (substring text 0 max-length) "...")
      text)))

(defun punch-line-icon ()
  "Return the icon for the music service."
  (let ((service (punch-line-get-music-service)))
    (cond
     ((eq service 'apple) (propertize (nerd-icons-faicon "nf-fa-music") 'face 'punch-line-music-apple-face))
     ((eq service 'spotify) (propertize (nerd-icons-faicon "nf-fa-spotify") 'face 'punch-line-music-spotify-face))
     (t ))))

(defun punch-line-start-music-info-timer ()
  "Start the timer for updating music information."
  (when (and punch-line-music-info (null punch-music-info-timer))
    (setq punch-music-info-timer
          (run-at-time 0 punch-music-info-update-interval #'punch-line-update-music-info-async))))

(defun punch-line-stop-music-info-timer ()
  "Stop the timer and clean up any running process."
  (when punch-music-info-timer
    (cancel-timer punch-music-info-timer)
    (setq punch-music-info-timer nil))
  (punch-line-cleanup-stale-process))

(defun punch-line-get-music-info ()
  "Get the current cached music info with a limited length."
  punch-music-info-cache)

(defun punch-line-music-info ()
  "Return the current music info for display in the mode-line."
  (when (and punch-line-music-info (punch-line-get-music-service))
    (punch-line-get-music-info)))

(when (eq system-type 'darwin)
  (add-hook 'emacs-startup-hook #'punch-line-start-music-info-timer)
  (add-hook 'kill-emacs-hook #'punch-line-stop-music-info-timer))

(provide 'punch-line-music)
;;; punch-line-music.el ends here
