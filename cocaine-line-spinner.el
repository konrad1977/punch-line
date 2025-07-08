;;; cocaine-line-spinner.el --- A customizable spinner/progress indicator -*- lexical-binding: t -*-
;; Copyright (C) 2025 Mikael Konradsson
;; Author:  Mikael Konradsson <mikael.konradsson@outlook.com>
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: progress, tools
;; URL: https://github.com/yourusername/spinner

;;; Commentary:
;; This package provides a customizable spinner/progress indicator
;; that can be used in mode-lines or any other part of Emacs.
;; It supports different spinner types and customizable update intervals.

;;; Code:

(require 'cl-lib)

(defgroup cocaine-line-spinner nil
  "Customizable cocaine-line-spinner for mode-line and other purposes."
  :group 'progress
  :prefix 'cocaine-line-)

(defcustom cocaine-line-spinner-types
  '((braille . ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])
    (horizontal-bar . ["─" "━"])
    (vertical-bar . ["┃" "┆" "┇" "┊" "┋"])
    (dots . ["⣾" "⣽" "⣻" "⢿" "⡿" "⣟" "⣯" "⣷"]))
  "Available cocaine-line-spinner animation types."
  :type '(alist :key-type symbol :value-type (vector string))
  :group 'cocaine-line-spinner)

(defcustom cocaine-line-spinner-interval 0.1
  "Number of seconds between cocaine-line-spinner updates."
  :type 'number
  :group 'cocaine-line-spinner)

(cl-defstruct (cocaine-line-spinner
               (:constructor cocaine-line-spinner-create)
               (:copier nil))
  (type 'braille)
  (frames (alist-get 'braille cocaine-line-spinner-types))
  (current-frame 0)
  (timer nil)
  (active nil)
  (update-fn nil))

(defun cocaine-line-spinner-next-frame (spinner)
  "Return the next frame in SPINNER's animation."
  (let* ((frames (cocaine-line-spinner-frames spinner))
         (current (cocaine-line-spinner-current-frame spinner))
         (next (% (1+ current) (length frames))))
    (setf (cocaine-line-spinner-current-frame spinner) next)
    (aref frames current)))

(defun cocaine-line-spinner-start (spinner)
  "Start SPINNER animation."
  (unless (cocaine-line-spinner-active spinner)
    (setf (cocaine-line-spinner-active spinner) t)
    (setf (cocaine-line-spinner-timer spinner)
          (run-with-timer 0 cocaine-line-spinner-interval
                         (lambda ()
                           (when (cocaine-line-spinner-update-fn spinner)
                             (funcall (cocaine-line-spinner-update-fn spinner)
                                    (cocaine-line-spinner-next-frame spinner))))))))

(defun cocaine-line-spinner-stop (spinner)
  "Stop SPINNER animation."
  (when (cocaine-line-spinner-active spinner)
    (when (cocaine-line-spinner-timer spinner)
      (cancel-timer (cocaine-line-spinner-timer spinner)))
    (setf (cocaine-line-spinner-active spinner) nil
          (cocaine-line-spinner-timer spinner) nil
          (cocaine-line-spinner-current-frame spinner) 0)))

;;;###autoload
(defun cocaine-line-spinner-create-for-mode-line (&optional type)
  "Create a spinner for use in the mode-line.
Optional TYPE specifies the spinner type from `cocaine-line-spinner-types'."
  (let ((spinner (cocaine-line-spinner-create :type type)))
    (setf (cocaine-line-spinner-update-fn spinner)
          (lambda (_)
            (force-mode-line-update)))
    spinner))

;;;###autoload
(defmacro cocaine-line-with-spinner (spinner &rest body)
  "Execute BODY with SPINNER active.
Automatically starts and stops the spinner."
  (declare (indent 1))
  `(progn
     (cocaine-line-spinner-start ,spinner)
     (unwind-protect
         (progn ,@body)
       (cocaine-line-spinner-stop ,spinner))))

;; Example usage in mode-line:
(defvar cocaine-line-mode-line-spinner (cocaine-line-spinner-create-for-mode-line))

;;;###autoload
(defmacro cocaine-line-with-async-update (spinner updating-sym &rest body)
  "Run BODY asynchronously with SPINNER animation.
UPDATING-SYM is used to track update status.
Returns spinner frame while updating, and last value of BODY when done."
  (declare (indent 2))
  `(if ,updating-sym
       (cocaine-line-spinner-next-frame ,spinner)
     (progn
       (setq ,updating-sym t)
       (cocaine-line-spinner-start ,spinner)
       (run-with-timer 0 nil
                      (lambda ()
                        ,@body
                        (setq ,updating-sym nil)
                        (cocaine-line-spinner-stop ,spinner)
                        (force-mode-line-update)))
       (cocaine-line-spinner-next-frame ,spinner))))

(provide 'cocaine-line-spinner)
;;; cocaine-line-spinner.el ends here 