;;; punch-line-colors.el --- A customized mode-line for Emacs with Evil status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (evil "1.0.0"))

;;; Commentary:

;;; Code:

(defcustom punch-height 7
  "Width of the box around mode line segments."
  :type 'integer
  :group 'punch-line)

;;; Evil faces
(defface punch-line-evil-normal-face
  `((t :foreground "#FFFFFF" :background "#2D4F67" :weight bold
       :box (:line-width ,punch-height :color "#2D4F67")))
  "Face for Evil normal state."
  :group 'punch-line)

(defface punch-line-evil-insert-face
  `((t :foreground "#333333" :background "#E6C384" :weight bold
       :box (:line-width ,punch-height :color "#E6C384")))
  "Face for Evil insert state."
  :group 'punch-line)

(defface punch-line-evil-visual-face
  `((t :foreground "#333333" :background "#D27E99" :weight bold
       :box (:line-width ,punch-height :color "#D27E99")))
  "Face for Evil visual state."
  :group 'punch-line)

(defface punch-line-evil-replace-face
  `((t :foreground "#333333" :background "#FF5D62" :weight bold
       :box (:line-width ,punch-height :color "#FF5D62")))
  "Face for Evil replace state."
  :group 'punch-line)

(defface punch-line-evil-emacs-face
  `((t :foreground "#333333" :background "#B0C4DE" :weight bold
       :box (:line-width ,punch-height :color "#B0C4DE")))
  "Face for Emacs state."
  :group 'punch-line)

(defface punch-line-macro-face
  `((t :foreground "#333333" :background "#B0C4DE" :weight bold
       :box (:line-width ,punch-height :color "#B0C4DE")))
  "Face for Emacs state."
  :group 'punch-line)

(defface punch-line-macro-recording-face
  `((t :foreground "#222233" :background "#FF5D62" :weight bold
       :box (:line-width ,punch-height :color "#FF5D62")))
  "Face for Emacs state."
  :group 'punch-line)

(defface punch-line-meow-motion-face
  `((t :foreground "#333333" :background "#B0C4DE" :weight bold
       :box (:line-width ,punch-height :color "#B0C4DE")))
  "Face for Emacs state."
  :group 'punch-line)

(defface punch-line-meow-keypad-face
  `((t :foreground "#333333" :background "#D27E99" :weight bold
       :box (:line-width ,punch-height :color "#D27E99")))
  "Face for Emacs state."
  :group 'punch-line)

(defface punch-line-meow-insert-exit-face
  `((t :foreground "#333333" :background "#E6C384" :weight bold
       :box (:line-width ,punch-height :color "#E6C384")))
  "Face for Emacs state."
  :group 'punch-line)

;;; Git faces
(defface punch-line-git-edited-face
  '((t :foreground "#658594"))
  "Face for edited Git files."
  :group 'punch-line)

(defface punch-line-git-added-face
  '((t :foreground "#50FA7B"))
  "Face for added Git files."
  :group 'punch-line)

(defface punch-line-git-removed-face
  '((t :foreground "#FF5D62"))
  "Face for removed Git files."
  :group 'punch-line)

(defface punch-line-git-conflict-face
  '((t :foreground "#FF79C6"))
  "Face for Git conflicts."
  :group 'punch-line)

(defface punch-line-inactive-face
  '((t :inherit mode-line-inactive))
  "Face for inactive mode-line elements."
  :group 'punch-line)

(defface punch-line-buffer-name-face
  '((t :inherit mode-line-buffer-id ))
  "Face for buffer name."
  :group 'punch-line)

(defface punch-line-major-mode-face
  '((t :inherit font-lock-doc-face :weight bold))
  "Face for major mode."
  :group 'punch-line)

(defface punch-line-position-face
  '((t :foreground "#FFA07A"))
  "Face for buffer position."
  :group 'punch-line)

(defface punch-line-time-face
  '((t :inherit mode-line-buffer-id))
  "Face for time display."
  :group 'punch-line)

(defface punch-line-git-face
  '((t :foreground "#a0a0ae"))
  "Standard face for Git information."
  :group 'punch-line)

(defface punch-line-project-face
  '((t :foreground "#FFA066" :weight bold))
  "Standard face for project information."
  :group 'punch-line)

(defface punch-line-lsp-icon-face
  '((t :inherit lsp-mode-line))
  "Standard face for project information."
  :group 'punch-line)

(defface punch-line-separator-face
  '((t :foreground "#54536D" :weight thin))
  "Face for the separator between sections in punch-line."
  :group 'punch-line)

(defface punch-line-what-am-i-doing-face
  '((t :inherit mode-line-buffer-id))
  "Face for the separator between sections in punch-line."
  :group 'punch-line)

(defface punch-line-what-am-i-doing-count-face
  '((t :inherit success))
  "Face for displaying current task in mode line."
  :group 'punch-line)

(require 'cl-lib)

(cl-defun adjust-colors (base-face &key background-adjust foreground-adjust
                                        background-color foreground-color
                                        weight slant underline overline strike-through
                                        box inverse-video stipple)
  "Generate a face specification based on BASE-FACE with adjustments.
Adjusts background by BACKGROUND-ADJUST percent (-100 to 100).
Adjusts foreground by FOREGROUND-ADJUST percent (-100 to 100).
Can set specific BACKGROUND-COLOR and/or FOREGROUND-COLOR.
Additional face attributes (WEIGHT, SLANT, etc.) can be specified."
  (let* ((base-face-attrs (face-all-attributes base-face nil))
         (bg (or background-color
                 (and background-adjust
                      (adjust-color (or (face-background base-face nil t) "unspecified")
                                    background-adjust))
                 (face-background base-face nil t)))
         (fg (or foreground-color
                 (and foreground-adjust
                      (adjust-color (or (face-foreground base-face nil t) "unspecified")
                                    foreground-adjust))
                 (face-foreground base-face nil t)))
         (spec `(,@base-face-attrs
                 ,@(when bg `(:background ,bg))
                 ,@(when fg `(:foreground ,fg))
                 ,@(when weight `(:weight ,weight))
                 ,@(when slant `(:slant ,slant))
                 ,@(when underline `(:underline ,underline))
                 ,@(when overline `(:overline ,overline))
                 ,@(when strike-through `(:strike-through ,strike-through))
                 ,@(when box `(:box ,box))
                 ,@(when inverse-video `(:inverse-video ,inverse-video))
                 ,@(when stipple `(:stipple ,stipple)))))
    spec))

(defun adjust-color (color percent)
  "Adjust COLOR by PERCENT (-100 to 100)."
  (if (string= color "unspecified")
      color
    (let* ((rgb (color-name-to-rgb color))
           (adjusted-rgb (mapcar (lambda (comp)
                                   (if (> percent 0)
                                       (min 1.0 (+ comp (* (- 1.0 comp) (/ percent 100.0))))
                                       (max 0.0 (+ comp (* comp (/ percent 100.0))))))
                                 rgb)))
      (apply 'color-rgb-to-hex adjusted-rgb))))

(provide 'punch-line-colors)
;;; punch-line-colors.el ends here
