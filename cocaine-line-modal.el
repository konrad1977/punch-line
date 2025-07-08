;;; cocaine-line-modal.el --- A customized mode-line for Emacs with optional Evil/Meow status -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (nerd-icons "1.0") (cocaine-line-colors "1.0"))

;;; Commentary:
;; This package provides mode-line customization with optional support for Evil and Meow modes.
;; If Evil or Meow are installed, their states will be displayed in the mode-line.

;;; Code:

(require 'nerd-icons)
(require 'cocaine-line-colors)

(defvar cocaine-line-height 1
  "Height of the mode-line.")

(defcustom cocaine-line-show-modal-section t
  "Show Evil and Meow modes in the mode-line when available."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-line-modal-use-fancy-icon t
  "Use fancy icons for Evil and Meow modes."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-line-modal-size 'small
  "Size of the mode-line."
  :type '(choice
          (const :tag "Small" small)
          (const :tag "Medium" medium)
          (const :tag "Large" large))
  :group 'cocaine-line)

(defcustom cocaine-line-modal-divider-style 'circle
  "Style of the divider icon."
  :type '(choice
          (const :tag "Arrow" arrow)
          (const :tag "Flame" flame)
          (const :tag "Ice" ice)
          (const :tag "Circle" circle))
  :group 'cocaine-line)

(defun cocaine-line-get-divider-icon-height ()
  "Get the height of the divider icon based on size."
  (pcase cocaine-line-modal-size
    ('small 1.0)
    ('medium 1.14)
    ('large 1.7)))

(defun cocaine-line-modal-height ()
  "Height of the mode-line based on size."
  (pcase cocaine-line-modal-size
    ('small 1)
    ('medium 4)
    ('large 12)))

(defun cocaine-line-get-divider-icon ()
  "Get the nerd-font icon name based on divider style."
  (pcase cocaine-line-modal-divider-style
    ('arrow "nf-pl-left_hard_divider")
    ('flame "nf-ple-flame_thick")
    ('ice "nf-ple-ice_waveform")
    ('circle "nf-ple-right_half_circle_thick")
    (_ "nf-pl-left_hard_divider")))

(defun cocaine-line-get-right-side-divider-icon ()
  "Get the nerd-font icon name based on divider style."
  (pcase cocaine-line-modal-divider-style
    ('arrow "nf-pl-right_hard_divider")
    ('flame "nf-ple-flame_thick_mirrored")
    ('ice "nf-ple-ice_waveform_mirrored")
    ('circle "nf-ple-left_half_circle_thick")
    (_ "nf-pl-left_hard_divider")))

(defcustom cocaine-evil-faces
  '((normal . cocaine-line-evil-normal-face)
    (insert . cocaine-line-evil-insert-face)
    (visual . cocaine-line-evil-visual-face)
    (replace . cocaine-line-evil-replace-face)
    (motion . cocaine-line-meow-motion-face)
    (keypad . cocaine-line-meow-keypad-face)
    (insert-exit . cocaine-line-meow-insert-exit-face))
  "Faces for different Evil and Meow states."
  :type '(alist :key-type symbol :value-type face)
  :group 'cocaine-line)

(defun cocaine-line-evil-available-p ()
  "Return t if Evil mode is available and enabled."
  (and (featurep 'evil)
       (bound-and-true-p evil-local-mode)
       (boundp 'evil-state)))

(defun cocaine-line-meow-available-p ()
  "Return t if Meow mode is available and enabled."
  (and (featurep 'meow)
       (bound-and-true-p meow-mode)
       (boundp 'meow-state)))

(defun cocaine-evil-status-inactive ()
  "Show Evil/Meow status with gray face for inactive mode-line."
  (when cocaine-line-show-modal-section
    (let* ((state (cond ((cocaine-line-evil-available-p) evil-state)
                        ((cocaine-line-meow-available-p) meow-state)
                        (t 'emacs)))
           (state-name (upcase (symbol-name state))))
      (propertize (format " %s " state-name)
                  'face 'cocaine-line-inactive-face))))

(cl-defun cocaine-evil-divider (&key icon icon-height background-face v-adjust)
  "Create a divider for the mode-line."
  (if cocaine-line-modal-use-fancy-icon
      (let* ((divider
              (propertize
               (if (fboundp 'nerd-icons-powerline)
                   (nerd-icons-powerline icon :v-adjust v-adjust)
                 "")
               'face `(:foreground ,background-face
                      :height ,icon-height))))
        divider)
    (propertize " " 'face `(:foreground ,background-face))))

(defvar-local cocaine-line--modal-cache nil
  "Cache for modal status.")

(defvar-local cocaine-line--modal-cache-state nil
  "Cached modal state.")

(defun cocaine-evil-status ()
  "Show Evil/Meow status with custom face and correct vertical alignment."
  (if cocaine-line-show-modal-section
      (let* ((current-state (cond ((cocaine-line-evil-available-p) evil-state)
                                 ((cocaine-line-meow-available-p) meow-state)
                                 (t 'emacs))))
        (if (and cocaine-line--modal-cache
                 (eq current-state cocaine-line--modal-cache-state))
            cocaine-line--modal-cache
          (let* ((state-face (or (cdr (assq current-state cocaine-evil-faces))
                                'cocaine-line-evil-emacs-face))
                 (state-name (upcase (symbol-name current-state)))
                 (background-face (face-background state-face nil t))
                 (height-adjust (/ (cocaine-line-modal-height) 2))
                 (divider (cocaine-evil-divider
                          :icon (cocaine-line-get-divider-icon)
                          :icon-height (cocaine-line-get-divider-icon-height)
                          :background-face background-face
                          :v-adjust (* (/ (cocaine-line-modal-height) 102.0 2.0) -1.0))))
            (setq cocaine-line--modal-cache-state current-state
                  cocaine-line--modal-cache
                  (concat
                   (propertize ""
                              'face `(:inherit ,state-face
                                     :box (:line-width ,height-adjust :color ,background-face)
                                     :height ,(cocaine-line-get-divider-icon-height)))
                   (propertize (format " %s " state-name)
                              'face `(:inherit ,state-face
                                     :box (:line-width ,height-adjust :color ,background-face)))
                   divider
                   " ")))))
    (propertize " " 'face 'cocaine-line-evil-normal-face)))

(defun cocaine-evil-mc-info ()
  "Show Evil MC information if available."
  (when (featurep 'evil-mc-vars)
    (let ((cursor-count (evil-mc-get-cursor-count))
          (icon (nerd-icons-octicon "nf-oct-pencil")))
      (if (> cursor-count 1)
          (propertize (format " %s %d " icon cursor-count)
                     'face '(:inherit cocaine-line-evil-replace-face))
        ""))))

(defun cocaine-iedit-info ()
  "Show iedit information if available."
  (when (featurep 'iedit)
    (let ((occurrence-count (or (and (boundp 'iedit-occurrences-overlays)
                                     (length iedit-occurrences-overlays))
                                0))
          (icon (nerd-icons-octicon "nf-oct-pencil")))
      (if (> occurrence-count 0)
          (propertize (format " %s %d " icon occurrence-count)
                      'face '(:inherit cocaine-line-evil-replace-face))
        ""))))

(defun cocaine-time-info ()
  "Show time with background matching the current evil state."
  (let* ((state (cond ((cocaine-line-evil-available-p) evil-state)
                      ((cocaine-line-meow-available-p) meow-state)
                      (t 'emacs)))
         (state-face (or (cdr (assq state cocaine-evil-faces))
                        'cocaine-line-evil-emacs-face))
         (background-color (face-background state-face nil t))
         (height-adjust (/ (cocaine-line-modal-height) 2))
         (divider (cocaine-evil-divider
                  :icon (cocaine-line-get-right-side-divider-icon)
                  :icon-height (cocaine-line-get-divider-icon-height)
                  :background-face background-color
                  :v-adjust (* (/ (cocaine-line-modal-height) 102.0 2.0) -1.0))))
    (concat
     divider
     (propertize ""
                'face `(:inherit ,state-face
                       :box (:line-width ,height-adjust :color ,background-color)
                       :height ,(cocaine-line-get-divider-icon-height)))
     (propertize (format " %s " (format-time-string "%H:%M"))
                'face `(:inherit ,state-face
                       :box (:line-width ,height-adjust :color ,background-color))))))

(provide 'cocaine-line-modal)
;;; cocaine-line-modal.el ends here 