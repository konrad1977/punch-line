;;; punch-line-modal.el --- A customized mode-line for Emacs with optional Evil/Meow status -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "28.1") (nerd-icons "1.0") (punch-line-colors "1.0"))

;;; Commentary:
;; This package provides mode-line customization with optional support for Evil and Meow modes.
;; If Evil or Meow are installed, their states will be displayed in the mode-line.

;;; Code:

(require 'nerd-icons)
(require 'punch-line-colors)

(defvar punch-line-height 1
  "Height of the mode-line.")

(defcustom punch-line-show-modal-section t
  "Show Evil and Meow modes in the mode-line when available."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-line-modal-use-fancy-icon t
  "Use fancy icons for Evil and Meow modes."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-line-modal-size 'small
  "Size of the mode-line."
  :type '(choice
          (const :tag "Small" small)
          (const :tag "Medium" medium)
          (const :tag "Large" large))
  :group 'punch-line)

(defcustom punch-line-modal-divider-style 'circle
  "Style of the divider icon."
  :type '(choice
          (const :tag "Arrow" arrow)
          (const :tag "Flame" flame)
          (const :tag "Ice" ice)
          (const :tag "Circle" circle))
  :group 'punch-line)

(defun punch-line-get-divider-icon-height ()
  "Get the height of the divider icon based on size."
  (pcase punch-line-modal-size
    ('small 1.0)
    ('medium 1.14)
    ('large 1.7)))

(defun punch-line-modal-height ()
  "Height of the mode-line based on size."
  (pcase punch-line-modal-size
    ('small 1)
    ('medium 4)
    ('large 12)))

(defun punch-line-get-divider-icon ()
  "Get the nerd-font icon name based on divider style."
  (pcase punch-line-modal-divider-style
    ('arrow "nf-pl-left_hard_divider")
    ('flame "nf-ple-flame_thick")
    ('ice "nf-ple-ice_waveform")
    ('circle "nf-ple-right_half_circle_thick")
    (_ "nf-pl-left_hard_divider")))

(defun punch-line-get-right-side-divider-icon ()
  "Get the nerd-font icon name based on divider style."
  (pcase punch-line-modal-divider-style
    ('arrow "nf-pl-right_hard_divider")
    ('flame "nf-ple-flame_thick_mirrored")
    ('ice "nf-ple-ice_waveform_mirrored")
    ('circle "nf-ple-left_half_circle_thick")
    (_ "nf-pl-left_hard_divider")))

(defcustom punch-evil-faces
  '((normal . punch-line-evil-normal-face)
    (insert . punch-line-evil-insert-face)
    (visual . punch-line-evil-visual-face)
    (replace . punch-line-evil-replace-face)
    (motion . punch-line-meow-motion-face)
    (keypad . punch-line-meow-keypad-face)
    (insert-exit . punch-line-meow-insert-exit-face))
  "Faces for different Evil and Meow states."
  :type '(alist :key-type symbol :value-type face)
  :group 'punch-line)

(defun punch-line-evil-available-p ()
  "Return t if Evil mode is available and enabled."
  (and (featurep 'evil)
       (bound-and-true-p evil-local-mode)
       (boundp 'evil-state)))

(defun punch-line-meow-available-p ()
  "Return t if Meow mode is available and enabled."
  (and (featurep 'meow)
       (bound-and-true-p meow-mode)
       (boundp 'meow-state)))

(defun punch-evil-status-inactive ()
  "Show Evil/Meow status with gray face for inactive mode-line."
  (when punch-line-show-modal-section
    (let* ((state (cond ((punch-line-evil-available-p) evil-state)
                        ((punch-line-meow-available-p) meow-state)
                        (t 'emacs)))
           (state-name (upcase (symbol-name state))))
      (propertize (format " %s " state-name)
                  'face 'punch-line-inactive-face))))

(cl-defun punch-evil-divider (&key icon icon-height background-face v-adjust)
  "Create a divider for the mode-line."
  (if punch-line-modal-use-fancy-icon
      (let* ((divider
              (propertize
               (if (fboundp 'nerd-icons-powerline)
                   (nerd-icons-powerline icon :v-adjust v-adjust)
                 "")
               'face `(:foreground ,background-face
                      :height ,icon-height))))
        divider)
    (propertize " " 'face `(:foreground ,background-face))))

(defvar-local punch-line--modal-cache nil
  "Cache for modal status.")

(defvar-local punch-line--modal-cache-state nil
  "Cached modal state.")

(defun punch-evil-status ()
  "Show Evil/Meow status with custom face and correct vertical alignment."
  (if punch-line-show-modal-section
      (let* ((current-state (cond ((punch-line-evil-available-p) evil-state)
                                 ((punch-line-meow-available-p) meow-state)
                                 (t 'emacs))))
        (if (and punch-line--modal-cache
                 (eq current-state punch-line--modal-cache-state))
            punch-line--modal-cache
          (let* ((state-face (or (cdr (assq current-state punch-evil-faces))
                                'punch-line-evil-emacs-face))
                 (state-name (upcase (symbol-name current-state)))
                 (background-face (face-background state-face nil t))
                 (height-adjust (/ (punch-line-modal-height) 2))
                 (divider (punch-evil-divider
                          :icon (punch-line-get-divider-icon)
                          :icon-height (punch-line-get-divider-icon-height)
                          :background-face background-face
                          :v-adjust (* (/ (punch-line-modal-height) 102.0 2.0) -1.0))))
            (setq punch-line--modal-cache-state current-state
                  punch-line--modal-cache
                  (concat
                   (propertize ""
                              'face `(:inherit ,state-face
                                     :box (:line-width ,height-adjust :color ,background-face)
                                     :height ,(punch-line-get-divider-icon-height)))
                   (propertize (format " %s " state-name)
                              'face `(:inherit ,state-face
                                     :box (:line-width ,height-adjust :color ,background-face)))
                   divider
                   " ")))))
    (propertize " " 'face 'punch-line-evil-normal-face)))

(defun punch-evil-mc-info ()
  "Show Evil MC information if available."
  (when (featurep 'evil-mc-vars)
    (let ((cursor-count (evil-mc-get-cursor-count))
          (icon (nerd-icons-octicon "nf-oct-pencil")))
      (if (> cursor-count 1)
          (propertize (format " %s %d " icon cursor-count)
                     'face '(:inherit punch-line-evil-replace-face))
        ""))))

(defun punch-iedit-info ()
  "Show iedit information if available."
  (when (featurep 'iedit)
    (let ((occurrence-count (or (and (boundp 'iedit-occurrences-overlays)
                                     (length iedit-occurrences-overlays))
                                0))
          (icon (nerd-icons-octicon "nf-oct-pencil")))
      (if (> occurrence-count 0)
          (propertize (format " %s %d " icon occurrence-count)
                      'face '(:inherit punch-line-evil-replace-face))
        ""))))

(defun punch-time-info ()
  "Show time with background matching the current evil state."
  (let* ((state (cond ((punch-line-evil-available-p) evil-state)
                      ((punch-line-meow-available-p) meow-state)
                      (t 'emacs)))
         (state-face (or (cdr (assq state punch-evil-faces))
                        'punch-line-evil-emacs-face))
         (background-color (face-background state-face nil t))
         (height-adjust (/ (punch-line-modal-height) 2))
         (divider (punch-evil-divider
                  :icon (punch-line-get-right-side-divider-icon)
                  :icon-height (punch-line-get-divider-icon-height)
                  :background-face background-color
                  :v-adjust (* (/ (punch-line-modal-height) 102.0 2.0) -1.0))))
    (concat
     " "
     divider
     (propertize (format-time-string " %H:%M   ")
                'face `(:inherit ,state-face
                       :background ,background-color)))))

(provide 'punch-line-modal)
;;; punch-line-modal.el ends here
