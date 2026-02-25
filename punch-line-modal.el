;;; punch-line-modal.el --- A customized mode-line for Emacs with optional Evil/Meow status -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "28.1") (nerd-icons "1.0"))

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

(defvar-local punch-line--modal-cache nil
  "Cache for modal status.")

(defvar-local punch-line--modal-cache-state nil
  "Cached modal state.")

(defcustom punch-line-modal-use-fancy-icon t
  "Use fancy icons for Evil and Meow modes."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-line-modal-use-padding t
  "Use padding around the modal state."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-line-modal-size 'small
  "Size of the mode-line."
  :type '(choice
          (const :tag "Small" small)
          (const :tag "Medium" medium)
          (const :tag "Large" large))
  :group 'punch-line)

(defcustom punch-line-modal-divider-style 'block
  "Style of the divider icon."
  :type '(choice
          (const :tag "Arrow" arrow)
          (const :tag "None" none)
          (const :tag "Flame" flame)
          (const :tag "Ice" ice)
          (const :tag "Circle" circle)
          (const :tag "Block" block))
  :group 'punch-line)

(defcustom punch-line-coloring-style 'full
  "Style of evil/modal state coloring in the mode-line.
`full'    - Colored block/pill with state name text and colored time (default).
`dot'     - A single colored dot (●) indicating the current state.
`minimal' - No evil state indicator and no colored time."
  :type '(choice
          (const :tag "Full colored block with state name" full)
          (const :tag "Colored dot indicator" dot)
          (const :tag "No evil coloring" minimal))
  :initialize #'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         ;; Invalidate modal cache in all buffers when style changes
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (setq punch-line--modal-cache nil
                   punch-line--modal-cache-state nil)))
         ;; Force mode-line update
         (when (fboundp 'punch-line-update)
           (punch-line-update t)))
  :group 'punch-line)

(defun punch-line-get-divider-icon-height ()
  "Get the height of the divider icon based on size."
  (pcase punch-line-modal-size
    ('small 1.0)
    ('medium 1.14)
    ('large 1.0)))

(defun punch-line-modal-height ()
  "Height of the mode-line based on size."
  (pcase punch-line-modal-size
    ('small 1)
    ('medium 5)
    ('large 20)))

(defun punch-line-get-divider-icon ()
  "Get the nerd-font icon name based on divider style."
  (pcase punch-line-modal-divider-style
    ('arrow "nf-pl-left_hard_divider")
    ('flame "nf-ple-flame_thick")
    ('ice "nf-ple-ice_waveform")
    ('circle "nf-ple-right_half_circle_thick")
    ('block nil)
    ('none nil)
    (_ "nf-pl-left_hard_divider")))

(defun punch-line-get-right-side-divider-icon ()
  "Get the nerd-font icon name based on divider style."
  (pcase punch-line-modal-divider-style
    ('arrow "nf-pl-right_hard_divider")
    ('flame "nf-ple-flame_thick_mirrored")
    ('ice "nf-ple-ice_waveform_mirrored")
    ('circle "nf-ple-left_half_circle_thick")
    ('block nil)
    ('none nil)
    (_ "nf-pl-right_hard_divider")))

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
  (if (and punch-line-modal-use-fancy-icon icon)
      (let* ((divider
              (propertize
               (if (and (fboundp 'nerd-icons-powerline) icon)
                   (nerd-icons-powerline icon :v-adjust v-adjust)
                 "")
               'face `(:foreground ,background-face
                      :height ,icon-height))))
        divider)
    ;; Check if section backgrounds are enabled - if so, return empty string instead of space
    (if (bound-and-true-p punch-line-section-backgrounds)
        ""
      (propertize " " 'face `(:foreground ,background-face)))))

(defun punch-line--height-spacer ()
  "Return an invisible propertized string that preserves mode-line height.
Uses the current `punch-line-modal-size' to calculate the box height."
  (let* ((height-adjust (max 1 (/ (punch-line-modal-height) 2)))
         (bg (or (face-background 'mode-line nil t)
                 (face-background 'default nil t)
                 "#000000")))
    (propertize " " 'face `(:box (:line-width ,height-adjust :color ,bg)
                             :foreground ,bg :background ,bg))))

(defvar-local punch-line--modal-cache nil
  "Cache for modal status.")

(defvar-local punch-line--modal-cache-state nil
  "Cached modal state.")

(defun punch-evil-status ()
  "Show Evil/Meow status with custom face and correct vertical alignment.
Respects `punch-line-coloring-style':
  `full'    - Colored block/pill with state name text and divider.
  `dot'     - A single colored dot (●) indicating the current state.
  `minimal' - No indicator shown."
  (if (and punch-line-show-modal-section
           (not (eq punch-line-coloring-style 'minimal)))
      (let* ((current-state (cond ((punch-line-evil-available-p) evil-state)
                                  ((punch-line-meow-available-p) meow-state)
                                  (t 'emacs))))
        (if (and punch-line--modal-cache
                 (eq current-state punch-line--modal-cache-state))
            punch-line--modal-cache
          (let* ((state-face (or (cdr (assq current-state punch-evil-faces))
                                 'punch-line-evil-emacs-face))
                 (background-color (face-background state-face nil t)))
            (setq punch-line--modal-cache-state current-state
                  punch-line--modal-cache
                  (pcase punch-line-coloring-style
                    ('dot
                     (propertize "●" 'face `(:foreground ,background-color)))
                     (_  ;; 'full (default)
                     (let* ((state-name (upcase (symbol-name current-state)))
                            (height-adjust (max 1 (/ (punch-line-modal-height) 2)))
                            (divider (punch-evil-divider
                                      :icon (punch-line-get-divider-icon)
                                      :icon-height (punch-line-get-divider-icon-height)
                                      :background-face background-color
                                      :v-adjust (* (/ (punch-line-modal-height) 102.0 2.0) -1.0))))
                       (concat
                        (propertize ""
                                    'face `(:inherit ,state-face
                                                     :box (:line-width ,height-adjust :color ,background-color)
                                                     :height ,(punch-line-get-divider-icon-height)))
                        (propertize (format " %s " state-name)
                                    'face `(:inherit ,state-face
                                                     :box (:line-width ,height-adjust :color ,background-color)))
                        divider
                        ""))))))))
    ""))

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
  "Show time with background matching the current evil state.
When `punch-line-coloring-style' is `dot' or `minimal', the time
is shown without evil state coloring."
  (if (eq punch-line-coloring-style 'full)
      ;; Full mode: colored time with evil state background and divider
      (let* ((state (cond ((punch-line-evil-available-p) evil-state)
                          ((punch-line-meow-available-p) meow-state)
                          (t 'emacs)))
             (state-face (or (cdr (assq state punch-evil-faces))
                             'punch-line-evil-emacs-face))
             (background-color (face-background state-face nil t))
             (height-adjust (max 1 (/ (punch-line-modal-height) 2)))
             (divider (punch-evil-divider
                       :icon (punch-line-get-right-side-divider-icon)
                       :icon-height (punch-line-get-divider-icon-height)
                       :background-face background-color
                       :v-adjust (* (/ (punch-line-modal-height) 102.0 2.0) -1.0))))
        (concat
         ;; Reduce spacing when section backgrounds are enabled
         (if (bound-and-true-p punch-line-section-backgrounds) "" " ")
         divider
         (propertize (format-time-string " %H:%M  ")
                     'face `(:inherit ,state-face
                                      :background ,background-color))))
    ;; Dot/minimal mode: plain time without evil coloring
    (propertize (format-time-string " %H:%M   ")
                'face 'punch-line-time-face)))

(provide 'punch-line-modal)
;;; punch-line-modal.el ends here
