;;; punch-line-spinner.el --- A customizable spinner/progress indicator -*- lexical-binding: t -*-
;; Copyright (C) 2025 Mikael Konradsson
;; Author:  Mikael Konradsson <mikael.konradsson@outlook.com>
;; Version: 1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: progress, tools
;; URL: https://github.com/yourusername/spinner

;;; Commentary:
;; This package provides a customizable spinner/progress indicator
;; that can be used in mode-lines or any other part of Emacs.
;; It supports different spinner types and customizable update intervals.

;;; Code:

(require 'cl-lib)

(defgroup punch-line-spinner nil
  "Customizable punch-line-spinner for mode-line and other purposes."
  :group 'progress
  :prefix "punch-line-")

(defcustom punch-line-spinner-types
  '((braille . ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])
    (horizontal-bar . ["─" "━"])
    (vertical-bar . ["┃" "┆" "┇" "┊" "┋"])
    (dots . ["⣾" "⣽" "⣻" "⢿" "⡿" "⣟" "⣯" "⣷"]))
  "Available punch-line-spinner animation types."
  :type '(alist :key-type symbol :value-type (vector string))
  :group 'punch-line-spinner)

(defcustom punch-line-spinner-interval 0.1
  "Number of seconds between punch-line-spinner updates."
  :type 'number
  :group 'punch-line-spinner)

(cl-defstruct (punch-line-spinner
               (:constructor punch-line-spinner-create)
               (:copier nil))
  (type 'braille)
  (frames (alist-get 'braille punch-line-spinner-types))
  (current-frame 0)
  (timer nil)
  (active nil)
  (update-fn nil))

(defun punch-line-spinner-next-frame (spinner)
  "Return the next frame in SPINNER's animation."
  (let* ((frames (punch-line-spinner-frames spinner))
         (current (punch-line-spinner-current-frame spinner))
         (next (% (1+ current) (length frames))))
    (setf (punch-line-spinner-current-frame spinner) next)
    (aref frames current)))

(defun punch-line-spinner-start (spinner)
  "Start SPINNER animation."
  (unless (punch-line-spinner-active spinner)
    (setf (punch-line-spinner-active spinner) t)
    (setf (punch-line-spinner-timer spinner)
          (run-with-timer 0 punch-line-spinner-interval
                         (lambda ()
                           (when (punch-line-spinner-update-fn spinner)
                             (funcall (punch-line-spinner-update-fn spinner)
                                    (punch-line-spinner-next-frame spinner))))))))

(defun punch-line-spinner-stop (spinner)
  "Stop SPINNER animation."
  (when (punch-line-spinner-active spinner)
    (when (punch-line-spinner-timer spinner)
      (cancel-timer (punch-line-spinner-timer spinner)))
    (setf (punch-line-spinner-active spinner) nil
          (punch-line-spinner-timer spinner) nil
          (punch-line-spinner-current-frame spinner) 0)))

;;;###autoload
(defun punch-line-spinner-create-for-mode-line (&optional type)
  "Create a spinner for use in the mode-line.
Optional TYPE specifies the spinner type from `punch-line-spinner-types'."
  (let ((spinner (punch-line-spinner-create :type type)))
    (setf (punch-line-spinner-update-fn spinner)
          (lambda (_)
            (force-mode-line-update)))
    spinner))

;;;###autoload
(defmacro punch-line-with-spinner (spinner &rest body)
  "Execute BODY with SPINNER active.
Automatically starts and stops the spinner."
  (declare (indent 1))
  `(progn
     (punch-line-spinner-start ,spinner)
     (unwind-protect
         (progn ,@body)
       (punch-line-spinner-stop ,spinner))))

;; Example usage in mode-line:
(defvar punch-line-mode-line-spinner (punch-line-spinner-create-for-mode-line))
(setq mode-line-format
      '("%e" mode-line-front-space
        (:eval (when (punch-line-spinner-active punch-line-mode-line-spinner)
                 (punch-line-spinner-next-frame punch-line-mode-line-spinner)))
        mode-line-buffer-identification))

;;;###autoload
(defmacro punch-line-with-async-update (spinner updating-sym &rest body)
  "Run BODY asynchronously with SPINNER animation.
UPDATING-SYM is used to track update status.
Returns spinner frame while updating, and last value of BODY when done."
  (declare (indent 2))
  `(if ,updating-sym
       (punch-line-spinner-next-frame ,spinner)
     (progn
       (setq ,updating-sym t)
       (punch-line-spinner-start ,spinner)
       (run-with-timer 0 nil
                      (lambda ()
                        ,@body
                        (setq ,updating-sym nil)
                        (punch-line-spinner-stop ,spinner)
                        (force-mode-line-update)))
       (punch-line-spinner-next-frame ,spinner))))

(provide 'punch-line-spinner)
;;; punch-line-spinner.el ends here
