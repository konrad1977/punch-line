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
(require 'punch-line-macro)
(require 'punch-line-music)
(require 'punch-line-modal)
(require 'punch-line-battery)
(require 'punch-line-misc)
(require 'punch-line-weather)
(require 'punch-line-term)
(require 'punch-line-systemmonitor)
(require 'punch-line-package)
(require 'punch-line-what-am-i-doing)

(require 'mode-line-hud)

(defgroup punch-line nil
  "Customizations for punch-line."
  :group 'mode-line)

(defvar punch-left-section-cache nil)
(defvar punch-right-section-cache nil)
(defvar punch-left-section-cache-time 0)
(defvar punch-right-section-cache-time 0)
(defvar punch-line-is-active nil)

(defcustom punch-left-section-update-interval 0.1
  "Minimum interval in seconds between left section updates."
  :type 'number
  :group 'punch-line)

(defcustom punch-right-section-update-interval 3
  "Minimum interval in seconds between right section updates."
  :type 'number
  :group 'punch-line)

(defcustom punch-line-left-separator "  "
  "Separator used between sections in the mode-line."
  :type 'string
  :group 'punch-line)

(defcustom punch-line-right-separator "  "
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
                        :box `(:line-width ,punch-height :color ,bg-color))))

(cl-defun punch-add-separator (&key str separator leftside (last nil) (face 'punch-line-separator-face))
  "Add a (SEPARATOR) around STR based on the arguments.
Add a separator after STR if it is not empty or last.  LAST
indicates if this is the last element.  FACE specifies which face
to use for the separator."
  (if (and str (not (string-empty-p str)) (not last))
      (if leftside
          (progn
            (if separator
                (concat str (propertize separator 'face face))
            (concat str (propertize punch-line-left-separator 'face face))))
        (progn
          (if separator
              (concat (propertize separator 'face face) str)
            (concat (propertize punch-line-left-separator 'face face) str))))
    str))

(defun punch-left-section ()
  "Create the left section of the mode-line with caching."
  (let ((current-time (float-time)))
    (when (or (null punch-left-section-cache)
	      (> (- current-time punch-left-section-cache-time) punch-left-section-update-interval))
      (setq punch-left-section-cache
	    (list (concat
		   (punch-macro-info)
		   (punch-evil-mc-info)
		   (punch-evil-status)
		   (punch-buffer-name)
		   (punch-add-separator :str (punch-major-mode) :separator "|")
		   (punch-add-separator :str (punch-project-info))
                   ;; (punch-add-separator :str (punch-org-info))
		   (punch-add-separator :str (punch-what-am-i-doing-info))
		   (punch-add-separator :str (punch-flycheck-mode-line))
		   (punch-add-separator :str (mode-line-segment-hud))
		   (punch-process-info))))
      (setq punch-left-section-cache-time current-time))
    punch-left-section-cache))

(defun punch-right-section ()
  "Create the right section of the mode-line with caching."
  (let ((current-time (float-time)))
    (when (or (null punch-right-section-cache)
              (> (- current-time punch-right-section-cache-time) punch-right-section-update-interval))
      (setq punch-right-section-cache
            (concat
             (punch-add-separator :str (punch-line-music-info) :separator punch-line-right-separator :leftside t)
	     (punch-add-separator :str (punch-line-col) :separator punch-line-right-separator :leftside t)
	     (punch-add-separator :str (punch-buffer-position) :separator punch-line-right-separator :leftside t)
             (punch-add-separator :str (punch-copilot-info) :separator punch-line-right-separator :leftside t)
             (punch-add-separator :str (punch-term-info) :separator punch-line-right-separator :leftside t)
	     (punch-add-separator :str (punch-misc-info) :separator punch-line-right-separator :leftside t)
             (punch-add-separator :str (punch-git-info) :separator punch-line-right-separator :leftside t)
             (punch-add-separator :str (punch-weather-info) :separator punch-line-right-separator :leftside t)
             (punch-add-separator :str (punch-battery-info) :separator punch-line-right-separator :leftside t)
             (punch-time-info)
	     ))
      (setq punch-right-section-cache-time current-time))
    punch-right-section-cache))

(defun punch-mode-line-inactive-format ()
  "Inactive format with Evil status and buffer name in gray."
  (list (concat
         (punch-evil-status-inactive)
         (propertize "|" 'face 'punch-line-inactive-face)
         (propertize (format " %s " (buffer-name))
                     'face 'punch-line-inactive-face))))

(defun punch-fill-to-right ()
  "Return whitespace to push the rest of the mode-line to the right."
  (let ((right-section (or (punch-right-section) ""))) ; Säkerställ att vi har en sträng
    (propertize " " 'display `((space :align-to (- right ,(string-width right-section)))))))

(defun punch-mode-line-format ()
  "Generate the mode-line format with improved caching."
  (if punch-line-is-active
      (list (punch-left-section)
            (punch-fill-to-right)  ; Ta bort ':eval och quote
            (punch-right-section))
    (punch-mode-line-inactive-format)))

(defun punch-update-mode-line (&optional _)
  "Update mode-line for all windows."
  (let ((active-window (selected-window)))
    (dolist (frame (frame-list))
      (dolist (window (window-list frame))
        (with-current-buffer (window-buffer window)
          (setq-local punch-line-is-active (eq window active-window))
          (force-mode-line-update))))))

(defun punch-set-mode-line ()
  "Set the mode-line format for punch-line."
  (setq-default mode-line-format '(:eval (punch-mode-line-format))))

(defun punch-register-hooks ()
  "Register hooks to update the mode-line."
  (add-hook 'post-command-hook #'punch-update-mode-line)
  (add-hook 'window-configuration-change-hook #'punch-update-mode-line)
  (add-hook 'focus-in-hook #'punch-update-mode-line)
  (add-hook 'focus-out-hook #'punch-update-mode-line)
  (add-hook 'window-buffer-change-functions #'punch-update-mode-line)  ; Add this hook
  (add-hook 'window-state-change-hook #'punch-update-mode-line)
  (add-hook 'after-load-theme-hook #'punch-update-inactive-face))

(defun punch-remove-hooks ()
  "Remove hooks to update the mode-line."
  (remove-hook 'post-command-hook #'punch-update-mode-line)
  (remove-hook 'window-configuration-change-hook #'punch-update-mode-line)
  (remove-hook 'focus-in-hook #'punch-update-mode-line)
  (remove-hook 'focus-out-hook #'punch-update-mode-line)
  (remove-hook 'window-buffer-change-functions #'punch-update-mode-line)  ; Remove this hook
  (remove-hook 'window-state-change-hook #'punch-update-mode-line)
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
