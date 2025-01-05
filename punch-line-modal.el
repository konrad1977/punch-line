;;; punch-line-modal.el --- A customized mode-line for Emacs with Evil status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (evil "1.0.0"))

;;; Commentary:


;;; Code:

(require 'nerd-icons)
(require 'punch-line-colors)

(defcustom punch-line-show-evil-modes t
  "Show Evil and Meow modes in the mode-line."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-line-modal-use-fancy-icon t
  "Use fancy icons for Evil and Meow modes."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-line-modal-divider-style 'flame
  "Style of the divider icon."
  :type '(choice
          (const :tag "Arrow" arrow)
          (const :tag "Flame" flame)
          (const :tag "Ice" ice)
          (const :tag "Circle" circle))
  :group 'punch-line)

(defun punch-line-get-divider-icon ()
  "Get the nerd-font icon name based on divider style."
  (pcase punch-line-modal-divider-style
    ('arrow "nf-pl-left_hard_divider")
    ('flame "nf-ple-flame_thick")
    ('ice "nf-ple-ice_waveform")
    ('hard "nf-ple-right_hard_divider_inverse")
    ('circle "nf-ple-right_half_circle_thick")
    (_ "nf-pl-left_hard_divider")))

(defcustom punch-evil-faces
  '((normal . punch-line-evil-normal-face)
    (insert . punch-line-evil-insert-face)
    (visual . punch-line-evil-visual-face)
    (replace . punch-line-evil-replace-face)
    (motion . punch-line-meow-motion-face)  ;; Meow motion face
    (keypad . punch-line-meow-keypad-face)  ;; Meow keypad face
    (insert-exit . punch-line-meow-insert-exit-face)) ;; Meow insert exit face
  "Faces for different Evil and Meow states."
  :type '(alist :key-type symbol :value-type face)
  :group 'punch-line)

(defun punch-evil-status-inactive ()
  "Show Evil/Meow status with gray face for inactive mode-line."
  (when punch-line-show-evil-modes
    (let* ((state (cond ((and (bound-and-true-p evil-local-mode) (boundp 'evil-state)) evil-state)
                        ((and (bound-and-true-p meow-mode) (boundp 'meow-state)) meow-state)
     (t 'emacs)))
           (state-name (upcase (symbol-name state))))
      (propertize (format " %s " state-name)
                  'face 'punch-line-inactive-face))))

(cl-defun punch-evil-divider (&key icon-height background-face)
  "Create a divider for the mode-line."
  (if punch-line-modal-use-fancy-icon
      (let* ((divider
              (propertize
               (if (fboundp 'nerd-icons-powerline)
                   (nerd-icons-powerline (punch-line-get-divider-icon) :v-adjust -0.08)
                 "")
               'face `(:foreground ,background-face
                       :height ,icon-height))))
        divider)
    (propertize " " 'face `(:foreground ,background-face))))

(defun punch-evil-status ()
  "Show Evil/Meow status with custom face and correct vertical alignment."
  (if punch-line-show-evil-modes
      (let* ((state (cond ((and (bound-and-true-p evil-local-mode) (boundp 'evil-state)) evil-state)
                         ((and (bound-and-true-p meow-mode) (boundp 'meow-state)) meow-state)
                         (t 'emacs)))
             (state-face (or (cdr (assq state punch-evil-faces))
                           'punch-line-evil-emacs-face))
             (state-name (upcase (symbol-name state)))
             (background-face (face-background state-face nil t))
             (divider (punch-evil-divider
                       :icon-height 1.48
                       :background-face background-face))
             )
        (concat
         (propertize (format " %s " state-name)
                     'face `(:inherit ,state-face
                             :box (:line-width ,punch-line-height :color ,background-face)))
                divider
         " "))
    (propertize " " 'face 'punch-line-evil-normal-face)))

(defun punch-evil-mc-info ()
  "Show Evil MC information."
  (require 'evil-mc-vars)
  (let ((cursor-count (evil-mc-get-cursor-count))
        (icon (nerd-icons-octicon "nf-oct-pencil")))
    (if (> cursor-count 1)
        (propertize (format " %s %d " icon cursor-count) 'face '(:inherit punch-line-evil-replace-face))
      "")))

(provide 'punch-line-modal)
;;; punch-line-modal.el ends here
