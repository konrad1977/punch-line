;;; mode-line-hud.el --- Package for interacting with mood-line -*- lexical-binding: t; -*-
;;; Commentary:
;; Package for interacting with mood-line.
;;; Code:
(defgroup mode-line-hud nil
  "Mode-line hud."
  :tag "mode-line-hud"
  :group 'mode-line-hud)

(defvar mode-line-hud--global-reset nil
  "Global flag to track if a reset has been requested.")

(defvar-local mode-line-segment-hud--text ""
  "Text to be displayed in the mode line HUD.")

(defvar-local mode-line-hud--active-timer nil
  "Active timer for the current notification.")

(defvar-local mode-line-hud--notification-id 0
  "Counter for tracking the current notification.")

(defvar mode-line-hud--debug nil
  "When non-nil, log debug information about HUD usage.")

(defvar mode-line-hud--call-log '()
  "Log of recent HUD calls with their callers.")

(defcustom mode-line-hud-log-size 100
  "Maximum number of entries to keep in the call log."
  :group 'mode-line-hud
  :type 'integer)

(defcustom show-in-echo-area nil
  "Show messages in echo area."
  :group 'mode-line-hud
  :type '(boolean))

(defcustom mode-line-hud-notification-debug nil
  "When non-nil, show notification metadata in echo area."
  :group 'mode-line-hud
  :type 'boolean)

(defun mode-line-hud--get-caller-context ()
  "Get detailed information about the calling context."
  (let ((i 1)
        (frames '())
        frame
        (max-frames 10))  ; Collect up to 10 relevant frames
    (while (and (< (length frames) max-frames)
                (setq frame (backtrace-frame i)))
      (when (and (car frame)           ; Frame is active
                 (cadr frame)           ; Has a function
                 (not (string-match-p "^mode-line-hud" (format "%s" (cadr frame))))  ; Not our internal functions
                 (not (memq (cadr frame) '(apply funcall))))  ; Skip apply and funcall frames
        (push (cadr frame) frames))
      (setq i (1+ i)))
    frames))

(defun mode-line-hud--format-caller-context (frames)
  "Format FRAMES into a readable caller context string."
  (if frames
      (mapconcat (lambda (frame) (format "%s" frame))
                 (reverse frames)
                 " -> ")
    "unknown"))

(defun mode-line-hud--get-caller ()
  "Get information about the calling function."
  (let ((i 1)
        (frame nil))
    (while (and (setq frame (backtrace-frame i))
                (not (and (car frame)
                         (symbolp (cadr frame))
                         (not (string-prefix-p "mode-line-hud" (symbol-name (cadr frame)))))))
      (setq i (1+ i)))
    (if frame
        (format "%s" (cadr frame))
      "unknown")))

(defun mode-line-hud--log-call (action &rest details)
  "Log an HUD ACTION with DETAILS."
  (let* ((caller-context (mode-line-hud--get-caller-context))
         (caller-string (mode-line-hud--format-caller-context caller-context))
         (entry (list :timestamp (current-time)
                     :action action
                     :caller caller-string
                     :details details
                     :context caller-context)))  ; Store full context for debugging
    (setq mode-line-hud--call-log
          (cons entry (take (1- mode-line-hud-log-size) mode-line-hud--call-log)))
    (when mode-line-hud--debug
      (message "HUD: %s called through %s with %s" action caller-string details))))

;;;###autoload
(defun mode-line-hud:show-call-log ()
  "Display the HUD call log in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*HUD Call Log*")
    (erase-buffer)
    (insert "Mode Line HUD Call Log:\n\n")
    (dolist (entry mode-line-hud--call-log)
      (let ((caller (plist-get entry :caller)))
        (insert (format-time-string "%Y-%m-%d %H:%M:%S" (plist-get entry :timestamp))
                " | "
                (format "%-50s" caller)  ; Increased width for caller context
                " | "
                (format "%-15s" (plist-get entry :action))
                " | "
                (format "%s" (plist-get entry :details))
                "\n")))
    (display-buffer (current-buffer))))

;;;###autoload
(defun mode-line-segment-hud ()
  "Return the current HUD text for the mode line."
  mode-line-segment-hud--text)

(defun mode-line-hud--update-display (text)
  "Update the HUD display with TEXT."
  ;; Ensure we're in the main thread
  (if (not (zerop (recursion-depth)))
      (run-with-timer 0 nil #'mode-line-hud--update-display text)
    (setq-local mode-line-segment-hud--text text)
    (when show-in-echo-area
      (message text))
    (force-mode-line-update 'all)))

(defun mode-line-hud--reset-timer()
  "Reset the active timer."
  (when mode-line-hud--active-timer
    (cancel-timer mode-line-hud--active-timer)
    (setq-local mode-line-hud--active-timer nil)))

(defun mode-line-hud--clear-all-buffers ()
  "Clear HUD messages in all buffers."
  (mode-line-hud--log-call 'clear-all-buffers)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (local-variable-p 'mode-line-segment-hud--text)
        (mode-line-hud--reset-timer)
        (mode-line-hud--update-display "")))))

;;;###autoload
(cl-defun mode-line-hud:update (&key message)
  "Update modeline with MESSAGE immediately.
This will override any existing message and cancel active timers."
  (mode-line-hud--log-call 'update :message message)
  (mode-line-hud--reset-timer)
  (setq-local mode-line-hud--notification-id 0)  ; Reset notification counter
  (mode-line-hud--update-display message))

;;;###autoload
(cl-defun mode-line-hud:updateWith (&key message delay)
  "Update modeline with MESSAGE after DELAY seconds."
  (mode-line-hud--log-call 'update-with :message message :delay delay)
  (mode-line-hud--reset-timer)
  (setq-local mode-line-hud--notification-id 0)  ; Reset notification counter
  (let ((buf (current-buffer)))
    (setq-local mode-line-hud--active-timer
                (run-with-timer delay nil
                              (lambda ()
                                (when (buffer-live-p buf)
                                  (with-current-buffer buf
                                    (mode-line-hud--update-display message)
                                    (setq-local mode-line-hud--active-timer nil))))))))

;;;###autoload
(cl-defun mode-line-hud:notification (&key message seconds (reset nil))
  "Display MESSAGE for SECONDS seconds in the mode line.
If RESET is non-nil, clear the message after SECONDS and clear all other HUD messages."
  (mode-line-hud--log-call 'notification :message message :seconds seconds :reset reset)
  (let* ((buf (current-buffer))
         (current-notif (setq-local mode-line-hud--notification-id
                                   (1+ mode-line-hud--notification-id))))
    (mode-line-hud--reset-timer)
    (mode-line-hud--update-display message)
    (when reset
      (setq mode-line-hud--global-reset t))
    (setq-local mode-line-hud--active-timer
                (run-with-timer seconds nil
                              (lambda ()
                                (when (and (buffer-live-p buf)
                                         ;; Only clear if this is still the active notification
                                         (eq current-notif mode-line-hud--notification-id))
                                  (with-current-buffer buf
                                    (if reset
                                        (progn
                                          (mode-line-hud--clear-all-buffers)
                                          (setq mode-line-hud--global-reset nil))
                                      (mode-line-hud--update-display ""))
                                    (setq-local mode-line-hud--active-timer nil))))))))

;;;###autoload
(defun mode-line-hud:toggle-debug ()
  "Toggle HUD debug logging."
  (interactive)
  (setq mode-line-hud--debug (not mode-line-hud--debug))
  (message "HUD debug logging %s" (if mode-line-hud--debug "enabled" "disabled")))

(provide 'mode-line-hud)
;;; mode-line-hud.el ends here
