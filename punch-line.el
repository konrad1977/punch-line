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

(defvar punch-line-is-active nil)

(defvar-local punch-line--cached-fill nil
  "Cached fill for the mode-line.")

(defvar-local punch-line--cached-right-width nil
  "Cached width of the right section of the mode-line.")

(defvar punch-line-active-window nil
  "Stores the currently active window.")

(defun punch-line-window-active-p ()
  "Return non-nil if the current window is active."
  (let ((current (get-buffer-window))
        (active punch-line-active-window))
    (eq current active)))

(defcustom punch-line-left-separator "  "
  "Separator used between sections in the mode-line."
  :type 'string
  :group 'punch-line)

(defcustom punch-line-right-separator "  "
  "Separator used between sections in the mode-line."
  :type 'string
  :group 'punch-line)

(defun punch-line-inactive-bg ()
  "Get the background color of the mode-line-inactive face."
  (face-background 'mode-line-inactive nil t))

(defun punch-update-inactive-face ()
  "Update the punch-line-inactive-face with the current mode-line-inactive background color."
  (let ((bg-color (punch-line-inactive-bg)))
    (set-face-attribute 'punch-line-inactive-face nil
                        :box `(:line-width ,(punch-line-modal-height) :color ,bg-color))))

(cl-defun punch-line-add-separator (&key str separator leftside (last nil) (face 'punch-line-separator-face))
  "Add a (SEPARATOR) around STR based on the arguments.
Add a separator after STR if it is not empty or last.  LAST
indicates if this is the last element.  FACE specifies which face
to use for the separator."
  (if (and str (not (string-empty-p str)) (not last))
      (if (not separator)
          str
        (let* ((height (punch-line-get-divider-icon-height))
               (divider (propertize separator
                                  'face `(:inherit ,face
                                         :height ,height))))
          (if leftside
              (concat str divider)
            (concat divider str))))
    str))

(defun punch-line-format-left ()
  "Create the left section of the mode-line with caching."
  (list (concat
         (punch-macro-info)
         (punch-evil-mc-info)
         (punch-evil-status)
         (punch-buffer-name)
         (punch-line-add-separator :str (punch-major-mode) :separator "|")
         (punch-line-add-separator :str (punch-project-info) :separator punch-line-left-separator)
         (punch-line-add-separator :str (punch-what-am-i-doing-info) :separator punch-line-left-separator)
         (punch-line-add-separator :str (punch-flycheck-info) :separator punch-line-left-separator)
         (punch-line-add-separator :str (mode-line-segment-hud) :separator punch-line-left-separator)
         (punch-process-info))))

(defun punch-line-format-right ()
  "Create the right section of the mode-line with caching."
  (concat
   (punch-line-add-separator :str (punch-line-music-info) :separator punch-line-right-separator :leftside t)
   (punch-line-add-separator :str (punch-system-monitor-info) :separator punch-line-right-separator :leftside t)
   (punch-line-add-separator :str (punch-line-col) :separator punch-line-right-separator :leftside t)
   (punch-line-add-separator :str (punch-buffer-position) :separator punch-line-right-separator :leftside t)
   (punch-line-add-separator :str (punch-copilot-info) :separator punch-line-right-separator :leftside t)
   (punch-line-add-separator :str (punch-term-info) :separator punch-line-right-separator :leftside t)
   (punch-line-add-separator :str (punch-misc-info) :separator punch-line-right-separator :leftside t)
   (punch-line-add-separator :str (punch-git-info) :separator punch-line-right-separator :leftside t)
   (punch-line-add-separator :str (punch-weather-info) :separator punch-line-right-separator :leftside t)
   (punch-battery-info)
   (punch-time-info)))

(defun punch-line-format-inactive ()
  "Inactive format with Evil status and buffer name in gray."
  (propertize (concat " " (punch-buffer-name)) 'face 'punch-line-inactive-face))

(defun punch-line-format ()
  "Generate the mode-line format."
  (if (punch-line-window-active-p)
      (list (punch-line-format-left)
            (punch-line-get-fill)
            (punch-line-format-right))
    (punch-line-format-inactive)))

(defun punch-line-update (&optional _)
  "Update mode-line for all windows."
  (let ((prev-active punch-line-active-window))
    (setq punch-line-active-window (selected-window))
    (force-mode-line-update t)))

(defun punch-line-set-mode-line ()
  "Set the mode-line format for punch-line."
  (setq-default mode-line-format '(:eval (punch-line-format))))

(defun punch-line-register-hooks ()
  "Register hooks to update the mode-line."
  (add-hook 'post-command-hook #'punch-line-update)
  (add-hook 'window-configuration-change-hook #'punch-line-update)
  (add-hook 'focus-in-hook #'punch-line-update)
  (add-hook 'focus-out-hook #'punch-line-update)
  (add-hook 'window-buffer-change-functions #'punch-line-update)  ; Add this hook
  (add-hook 'window-state-change-hook #'punch-line-update)
  (add-hook 'window-size-change-functions (lambda (_) (punch-line-invalidate-fill-cache)))
  (add-hook 'after-load-theme-hook #'punch-update-inactive-face))

(defun punch-line-remove-hooks ()
  "Remove hooks to update the mode-line."
  (remove-hook 'post-command-hook #'punch-line-update)
  (remove-hook 'window-configuration-change-hook #'punch-line-update)
  (remove-hook 'focus-in-hook #'punch-line-update)
  (remove-hook 'focus-out-hook #'punch-line-update)
  (remove-hook 'window-buffer-change-functions #'punch-line-update)  ; Remove this hook
  (remove-hook 'window-state-change-hook #'punch-line-update)
  (remove-hook 'window-size-change-functions (lambda (_) (punch-line-invalidate-fill-cache)))
  (remove-hook 'after-load-theme-hook #'punch-update-inactive-face))

(define-minor-mode punch-line-mode
  "Activate Punch Line mode."
  :group 'punch-line
  :global t
  :lighter nil
  (if punch-line-mode
      (progn
        (punch-line-set-mode-line)
        (punch-line-register-hooks)
        (punch-update-inactive-face)
        (punch-line-update))
    (setq-default mode-line-format (default-value 'mode-line-format))
    (punch-line-remove-hooks)
    (force-mode-line-update t)))

(defun punch-line-calculate-fill (right-section)
  "Calculate the fill space needed to right-align the RIGHT-SECTION."
  (let ((right-width (string-width (or right-section ""))))
    (setq punch-line--cached-right-width right-width)
    (propertize " " 'display
                `((space :align-to (- right ,(- right-width 1)))))))

(defun punch-line-get-fill ()
  "Get the fill space needed to right-align content with caching."
  (let* ((right-section (punch-line-format-right))
         (current-width (string-width (or right-section ""))))
    (if (and punch-line--cached-fill
             punch-line--cached-right-width
             (= current-width punch-line--cached-right-width))
        punch-line--cached-fill
      (setq punch-line--cached-fill
            (punch-line-calculate-fill right-section)))))

(defun punch-line-invalidate-fill-cache ()
  "Invalidate the fill cache."
  (setq punch-line--cached-fill nil
        punch-line--cached-right-width nil))

;; Add this to your cache invalidation logic
(defun punch-line-invalidate-caches ()
  "Invalidate all caches."
  (punch-line-invalidate-cache)  ; Your existing cache invalidation
  (punch-line-invalidate-fill-cache))

(provide 'punch-line)
;;; punch-line.el ends here
