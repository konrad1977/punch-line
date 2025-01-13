
;;; punch-line-term.el --- A customized mode-line for Emacs with modal status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs with Modal status,
;; configurable colors, and the ability to customize displayed information.

;;; Code:

(require 'nerd-icons)

(defface punch-term-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for the terminal icon in the punch line."
  :group 'punch-line)

(defcustom punch-show-term-info t
  "Flag to indicate if the terminal is hidden."
  :type 'boolean
  :group 'punch-line)

(defvar punch-term-process-name nil
  "Name of the current process running in the terminal.")

(defun vterm-buffer-is-running ()
  "Check if *vterm* buffer is running."
  (get-buffer "*vterm*"))

(defun vterm-visible-p ()
  "Check if *vterm* buffer is visible."
  (let ((vterm-buffer (get-buffer "*vterm*")))
    (and vterm-buffer
         (get-buffer-window vterm-buffer 'visible))))

(defun punch-vterm-last-command ()
  "Return the last command executed in the vterm buffer, avoiding navigation commands."
  (let ((buffer (get-buffer "*vterm*")))
    (when (and buffer (with-current-buffer buffer (derived-mode-p 'vterm-mode)))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-max))
          (let ((case-fold-search nil)  ; Make search case-sensitive
                (limit (max (point-min) (- (point-max) 3000))))  ; Limit the search to the last 3000 characters
            (catch 'result
  (while (re-search-backward " [↱±]\\s-+\\(.+\\)$" limit t)
                (let* ((full-command (string-trim (match-string-no-properties 1)))
                       (cleaned-command (replace-regexp-in-string "[^a-zA-Z ]" "" full-command))
                       (first-word (car (split-string cleaned-command))))
                  ;; Skip navigation commands
                  (if (and first-word (not (member first-word '("cd" "ls" "pwd"))))
                      (throw 'result (car (split-string cleaned-command)))
                    (message "Skipped command: %s" first-word)))))))))))

(defun punch-term-info ()
  "Return the terminal icon for the mode-line."
  (when (and (vterm-buffer-is-running) (not (vterm-visible-p)))
    (let ((last-command (punch-vterm-last-command)))
      (propertize
       (if last-command
           (format "%s %s" (nerd-icons-devicon "nf-dev-terminal") last-command)
         (nerd-icons-devicon "nf-dev-terminal"))
       'face 'punch-term-face))))

(provide 'punch-line-term)
;;; punch-line-term.el ends here
