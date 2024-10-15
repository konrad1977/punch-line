;;; punch-line.el --- A customized mode-line for Emacs with Evil status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (evil "1.0.0"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs with Evil status,
;; configurable colors, and the ability to customize displayed information.

;;; Code:
(require 'cl-lib)

(require 'punch-line-colors)
(require 'punch-line-vc)
(require 'punch-line-music)
(require 'punch-line-modal)
(require 'punch-line-battery)
(require 'punch-line-misc)
(require 'punch-line-weather)

(require 'mode-line-hud)

(defvar-local punch-line-is-active nil
  "Indicates if the current window is active.")

(defgroup punch-line nil
  "Customizations for punch-line."
  :group 'mode-line)

(defcustom punch-line-separator " | "
  "Separator used between sections in the mode-line."
  :type 'string
  :group 'punch-line)

(defcustom punch-line-separator-face 'punch-line-separator-face
  "Face for the separator between sections."
  :type 'face
  :group 'punch-line)

(defun punch-get-mode-line-inactive-bg ()
  "Get the background color of the mode-line-inactive face."
  (face-background 'mode-line-inactive nil t))

(defun punch-update-inactive-face ()
  "Update the punch-line-inactive-face with the current mode-line-inactive background color."
  (let ((bg-color (punch-get-mode-line-inactive-bg)))
    (set-face-attribute 'punch-line-inactive-face nil
                        :box `(:line-width 8 :color ,bg-color))))

(cl-defun punch-add-separator (&key str separator leftside (last nil) (face 'punch-line-separator-face))
  "Add a separator after STR if it is not empty or last."
  "Add a separator after STR if it is not empty or last.
LAST indicates if this is the last element.
FACE specifies which face to use for the separator."
  (if (and str (not (string-empty-p str)) (not last))
      (if leftside
          (progn
            (if separator
                (concat str (propertize separator 'face face))
              (concat str (propertize punch-line-separator 'face face))))
        (progn
          (if separator
              (concat (propertize separator 'face face) str)
            (concat (propertize punch-line-separator 'face face) str))
          ))
    str))

(defun punch-left-section ()
  "Create the left section of the mode-line."
  (let ((left-section (list (concat (punch-evil-status)
                                    (punch-evil-mc-info)
                                    (punch-line-spacer)
                                    (punch-buffer-name)
                                    (punch-add-separator :str (punch-major-mode) :separator "|")
                                    (punch-add-separator :str (punch-eglot-info))
                                    (punch-add-separator :str (mode-line-segment-hud))
                                    (punch-process-info)
                                    ))))
    left-section))

(defun punch-right-section ()
  "Create the right section of the mode-line."
  (let ((right-section (concat
                        (punch-add-separator :str (punch-line-music-info) :leftside t)
                        (punch-line-col)
                        (punch-add-separator :str (punch-flycheck-mode-line) :leftside t)
                        (punch-buffer-position)
                        (punch-add-separator :str (punch-copilot-info) :leftside t)
                        (punch-misc-info)
                        (punch-add-separator :str (punch-weather-info) :leftside t)
                        (punch-add-separator :str (punch-git-info) :leftside t)
                        (punch-add-separator :str (punch-battery-info) :leftside t)
                        (punch-time-info)
                        )))
    (list (propertize " " 'display `((space :align-to (- right ,(string-width right-section)))))
          right-section)))

(defun punch-mode-line-inactive-format ()
  "Inactive format with Evil status and buffer name in gray."
  (list (concat
         (punch-evil-status-inactive)
         (propertize "|" 'face 'punch-line-inactive-face)
         (propertize (format " %s " (buffer-name))
                     'face 'punch-line-inactive-face))))

(defun punch-mode-line-format ()
  "Generate the format for punch-line mode-line."
  (let ((left (punch-left-section))
        (right (punch-right-section)))
    (if punch-line-is-active
        (append left right)
      (punch-mode-line-inactive-format))))

(defun punch-update-mode-line (&optional _)
  "Update mode-line for all windows."
  (let ((active-window (selected-window)))
    (dolist (frame (frame-list))
      (dolist (window (window-list frame))
        (with-current-buffer (window-buffer window)
          (setq-local punch-line-is-active (eq window active-window))
          (force-mode-line-update window))))))

(defun punch-set-mode-line ()
  "Set the mode-line format for punch-line."
  (setq-default mode-line-format '(:eval (punch-mode-line-format))))

(defun punch-register-hooks ()
  "Register hooks to update the mode-line."
  (add-hook 'post-command-hook #'punch-update-mode-line)
  (add-hook 'window-configuration-change-hook #'punch-update-mode-line)
  (add-hook 'focus-in-hook #'punch-update-mode-line)
  (add-hook 'focus-out-hook #'punch-update-mode-line)
  (add-hook 'window-state-change-hook #'punch-update-mode-line)  ; Add this hook
  (add-hook 'after-load-theme-hook #'punch-update-inactive-face))

(defun punch-remove-hooks ()
  "Remove hooks to update the mode-line."
  (remove-hook 'post-command-hook #'punch-update-mode-line)
  (remove-hook 'window-configuration-change-hook #'punch-update-mode-line)
  (remove-hook 'focus-in-hook #'punch-update-mode-line)
  (remove-hook 'focus-out-hook #'punch-update-mode-line)
  (remove-hook 'window-state-change-hook #'punch-update-mode-line)  ; Remove this hook
  (remove-hook 'after-load-theme-hook #'punch-update-inactive-face))

(define-minor-mode punch-line-mode
  "Activate Punch Line mode."
  :group 'punch-line
  :global t
  :lighter nil
  (if punch-line-mode
      (progn
        (punch-set-mode-line)
        (punch-register-hooks)
        (punch-update-inactive-face)
        (punch-update-mode-line))
    (setq-default mode-line-format (default-value 'mode-line-format))
    (punch-remove-hooks)
    (force-mode-line-update t)))

(provide 'punch-line)
;;; punch-line.el ends here
