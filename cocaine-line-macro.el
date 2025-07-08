;;; cocaine-line-macro.el --- A customized mode-line for Emacs with modal status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs with modal status,
;; configurable colors, and the ability to customize displayed information.

;;; Code:
(require 'cocaine-line-colors)

(defvar cocaine-macro-recording-symbol "●"
  "Symbol to indicate macro recording is in progress.")

(defvar cocaine-macro-recorded-symbol "■"
  "Symbol to indicate a macro has been recorded but is not currently recording.")

(defvar cocaine-macro-count 0
  "Count of actions in the current or last recorded macro.")

(defcustom cocaine-show-macro t
  "Non-nil means show macro, nil means hide macro."
  :type 'boolean
  :group 'cocaine-line)

(defun cocaine-macro-start-hook ()
  "Hook function called when macro recording starts."
  (setq cocaine-macro-count 0))

(defun cocaine-macro-end-hook ()
  "Hook function called when macro recording ends."
  nil)

(defun cocaine-macro-exec-hook ()
  "Hook function called after each command during macro recording."
  (setq cocaine-macro-count (1+ cocaine-macro-count)))

(defun cocaine-macro-info ()
  "Return a string with information about the current macro status."
  (let ((info ""))
    (cond
     (defining-kbd-macro
      (setq info (propertize (format " %s %d " cocaine-macro-recording-symbol cocaine-macro-count) 'face 'cocaine-line-macro-recording-face)))
     ((and last-kbd-macro (> cocaine-macro-count 0))
      (setq info (format " %s %d " cocaine-macro-recorded-symbol cocaine-macro-count)))
     (t
      (setq info "")))
    info))

(when (bound-and-true-p cocaine-show-macro)
  (add-hook 'kmacro-start-hook #'cocaine-macro-start-hook)
  (add-hook 'kmacro-end-hook #'cocaine-macro-end-hook)
  (add-hook 'kmacro-exec-hook #'cocaine-macro-exec-hook))

(provide 'cocaine-line-macro)
;;; cocaine-line-macro.el ends here 