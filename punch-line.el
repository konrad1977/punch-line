;;; punch-line.el --- A customized mode-line for Emacs with Evil status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (evil "1.0.0"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs with Evil status,
;; configurable colors, and the ability to customize displayed information.

;;; Code:
(require 'async)
(require 'all-the-icons)
(require 'battery)
(require 'cl-lib)
(require 'evil)
(require 'nerd-icons)
(require 'project)

(require 'punch-line-colors)
(require 'punch-line-vc)
(require 'punch-line-music)
(require 'punch-line-modal)
(require 'mode-line-hud)

(unless (bound-and-true-p battery-status-function)
  (battery-update-handler))

(defvar-local punch-line-is-active nil
  "Indicates if the current window is active.")

(defgroup punch-line nil
  "Customizations for punch-line."
  :group 'mode-line)

(defcustom punch-right-padding 0
  "Number of spaces to add after the time."
  :type 'integer
  :group 'punch-line)

(defcustom punch-show-column-info nil
  "If set to t, show column information."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-buffer-position nil
  "If set to t, show buffer position."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-battery-info t
  "If set to t, show battery icons with nerdicons."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-battery-show-percentage t
  "When non-nil, display battery percentage as text after the icon."
  :type 'boolean
  :group 'punch)

(defcustom punch-show-use-nerd-icons t
  "If set to t, show file icons with nerdicons."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-processes-info t
  "If set to t, show active processes."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-misc-info nil
  "If set to t, show misc processes."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-copilot-info t
  "If set to t, show copilot icon."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-line-show-time-info t
  "If set to t, show time."
  :type 'boolean
  :group 'punch-line)

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

(defun punch-flycheck-mode-line ()
  "Custom flycheck mode-line with icons and counts."
  (when (and (bound-and-true-p flycheck-mode)
             (or flycheck-current-errors
                 (eq 'running flycheck-last-status-change)))
    (let* ((count (flycheck-count-errors flycheck-current-errors))
           (info (or (cdr (assq 'info count)) 0))
           (warning (or (cdr (assq 'warning count)) 0))
           (error (or (cdr (assq 'error count)) 0)))
      (concat
       (when (> info 0)
         (propertize (format "%s %d" (nerd-icons-codicon "nf-cod-lightbulb") info)
                     'face '(:inherit success)))
       (when (> warning 0)
         (propertize (format " %s %d" (nerd-icons-codicon "nf-cod-warning") warning)
                     'face '(:inherit warning)))
       (when (> error 0)
         (propertize (format " %s %d" (nerd-icons-codicon "nf-cod-error") error)
                     'face '(:inherit error)))))))

(defun punch-process-info ()
  "Show information about active processes."
  (when punch-show-processes-info
    (let ((process-info (format-mode-line mode-line-process)))
        process-info)))

(defun punch-misc-info ()
  "Show information about misc info."
  (when punch-show-misc-info
    (let ((misc-info (format-mode-line mode-line-misc-info)))
      (unless (string-blank-p misc-info)
        (string-trim misc-info)))))

(defun punch-eglot-info ()
  "Return a string representing the current Eglot status for the mode line using nerd-icons."
  (if (bound-and-true-p eglot--managed-mode)
      (let* ((server (eglot-current-server))
             (nick (and server (eglot--project-nickname server)))
             (icon (propertize (nerd-icons-codicon "nf-cod-pulse")
                               'face 'punch-line-eglot-icon-face)))
        (if server
            (concat (propertize (or nick "") 'face 'punch-line-project-face) " " icon " ")
          icon))
    (punch-project-info)))

(defun punch-project-name ()
  "Get the project name if any."
  (or
   (and (fboundp 'project-name)
        (project-current)
        (project-name (project-current)))
   (and (fboundp 'projectile-project-name)
        (projectile-project-name))))

(defun punch-project-info ()
  "Show project information."
  (let ((project (punch-project-name)))
    (propertize project 'face 'punch-line-project-face)))

;; Custom functions for left section
(defun punch-buffer-name ()
  "Show buffer name with custom face and icon (if available)."
  (let* ((file-name (buffer-file-name))
         (icon (when (and punch-show-use-nerd-icons file-name)
                 (nerd-icons-icon-for-file file-name t)))
         (buffer-name (file-name-sans-extension
                       (substring-no-properties (format-mode-line "%b ")))))
    (if icon
        (concat icon " " (propertize buffer-name 'face 'punch-line-buffer-name-face))
      (propertize buffer-name 'face 'punch-line-buffer-name-face))))

(defun punch-major-mode ()
  "Show major mode with custom face."
  (propertize (substring-no-properties (format-mode-line mode-name))
              'face 'punch-line-major-mode-face))

(defun punch-line-col ()
  "Show line and column with custom face."
  (when punch-show-column-info
    (propertize "[%l:%c]" 'face 'punch-line-position-face)))

(defun punch-buffer-position ()
  "Show buffer position percentage with custom face."
  (when punch-show-buffer-position
    (propertize "%p%" 'face 'punch-line-position-face)))

(defun punch-copilot-info ()
  "HUD for Copilot."
  (when (and punch-show-copilot-info (bound-and-true-p copilot-mode))
    (propertize " " 'face '(:inherit success))))

(defun punch-line-spacer ()
  "Show an empty string."
  (propertize " "))

(defun punch-time-info ()
  "Show time with custom face."
  (when punch-line-show-time-info
    (propertize (format-time-string "%H:%M") 'face 'punch-line-time-face)))

(defun punch-battery-info ()
  "Show battery percentage or charging status using text and nerd-font icons on macOS with face-based coloring."
  (when (and punch-show-battery-info
             (bound-and-true-p display-battery-mode))
    (let* ((battery-plist (funcall battery-status-function))
           (percentage (string-to-number (battery-format "%p" battery-plist)))
           (status (battery-format "%B" battery-plist))
           (charging (or (string= status "AC") (string= status "charging")))
           (icon (cond
                  (charging (nerd-icons-faicon "nf-fa-plug"))
                  ((>= percentage 87.5) (nerd-icons-faicon "nf-fa-battery"))
                  ((>= percentage 62.5) (nerd-icons-faicon "nf-fa-battery_3"))
                  ((>= percentage 37.5) (nerd-icons-faicon "nf-fa-battery_2"))
                  ((>= percentage 12.5) (nerd-icons-faicon "nf-fa-battery_1"))
                  (t (nerd-icons-faicon "nf-fa-battery_0"))))
           (face (cond
                  (charging 'success)
                  ((<= percentage 10) 'error)
                  ((<= percentage 30) 'warning)
                  (t 'success)))
           (percentage-text (if punch-battery-show-percentage
                                (format " %d%%" percentage)
                              "")))
      (if (and percentage status)
          (propertize
           (format "%s %s%%"
                   icon
                   percentage-text)
           'face face)
        "No battery info"))))

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
