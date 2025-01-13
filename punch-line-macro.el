;;; punch-line-macro.el --- A customized mode-line for Emacs with modal status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs with modal status,
;; configurable colors, and the ability to customize displayed information.

;;; Code:
(require 'punch-line-colors)

(defvar punch-macro-recording-symbol "●"
  "Symbol to indicate macro recording is in progress.")

(defvar punch-macro-recorded-symbol "■"
  "Symbol to indicate a macro has been recorded but is not currently recording.")

(defvar punch-macro-count 0
  "Count of actions in the current or last recorded macro.")

(defcustom punch-show-macro t
  "Non-nil means show macro, nil means hide macro."
  :type 'boolean
  :group 'punch-line)

(defun punch-macro-start-hook ()
  "Hook function called when macro recording starts."
  (setq punch-macro-count 0))

(defun punch-macro-end-hook ()
  "Hook function called when macro recording ends."
  nil)

(defun punch-macro-exec-hook ()
  "Hook function called after each command during macro recording."
  (setq punch-macro-count (1+ punch-macro-count)))

(defun punch-macro-info ()
  "Return a string with information about the current macro status."
  (let ((info ""))
    (cond
     (defining-kbd-macro
      (setq info (propertize (format " %s %d " punch-macro-recording-symbol punch-macro-count) 'face 'punch-line-macro-recording-face)))
     ((and last-kbd-macro (> punch-macro-count 0))
      (setq info (format " %s %d " punch-macro-recorded-symbol punch-macro-count)))
     (t
      (setq info "")))
    info))

(when (bound-and-true-p punch-show-macro)
  (add-hook 'kmacro-start-hook #'punch-macro-start-hook)
  (add-hook 'kmacro-end-hook #'punch-macro-end-hook)
  (add-hook 'kmacro-exec-hook #'punch-macro-exec-hook))

(provide 'punch-line-macro)
;;; punch-line-macro.el ends here
