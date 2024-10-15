;;; mode-line-hud.el --- package for interacting with the mood-line -*- lexical-binding: t; -*-

;;; commentary:

;;; package for interacting with mood-line.

;;; code:

(defgroup mode-line-hud nil
  "Mode-line hud."
  :tag "mode-line-hud"
  :group 'mode-line-hud)

(defvar-local mode-line-segment-hud--text "")

(defcustom show-in-echo-area nil
  "Show messages in echo area."
  :group 'mode-line-hud
  :type '(boolean))

;;;###autoload
(defun mode-line-segment-hud ()
  "Return the number of active multiple-cursors."
  mode-line-segment-hud--text)

;;;###autoload
(cl-defun mode-line-hud:update (&key message)
  "Update modeline as (MESSAGE)."
  (run-with-timer 0.05 nil
                  (lambda ()
                    (setq mode-line-segment-hud--text message)
                    (when show-in-echo-area
                      (message message))
                    (force-mode-line-update))))

;;;###autoload
(cl-defun mode-line-hud:updateWith (&key message delay)
  "Update modeline as (MESSAGE DELAY)."
  (run-with-timer delay nil
                  (lambda ()
                    (setq mode-line-segment-hud--text message)
                    (when show-in-echo-area
                      (message message))
                    (force-mode-line-update))))

;;;###autoload
(cl-defun mode-line-hud:notification (&key message seconds (reset nil))
  "Update modeline as (MESSAGE SECONDS (as RESET).)."
  (run-with-timer 0.025 nil
                  (lambda ()
                    (if reset
                        (mode-line-hud:reset :message "" :delay seconds)
                      (mode-line-hud:reset :message mode-line-segment-hud--text :delay seconds))
                    (setq mode-line-segment-hud--text message)
                    (when show-in-echo-area
                      (message message))
                    (force-mode-line-update))))

(cl-defun mode-line-hud:reset (&key message delay)
  "Reset to previous MESSAGE DELAY."
  (run-with-timer delay nil
                  (lambda ()
                    (setq mode-line-segment-hud--text message)
                    (when show-in-echo-area
                      (message message))
                    (force-mode-line-update))))

(provide 'mode-line-hud)
;;; mode-line-hud.el ends here
