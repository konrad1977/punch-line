;;; punch-line-colors.el --- A customized mode-line for Emacs with Evil status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (evil "1.0.0"))

;;; Commentary:


;;; Code:

;;; Evil faces
(defface punch-line-evil-normal-face
  '((t :foreground "#FFFFFF" :background "#2D4F67" :weight bold
       :box (:line-width 8 :color "#2D4F67")))
  "Face for Evil normal state."
  :group 'punch-line)

(defface punch-line-evil-insert-face
  '((t :foreground "#333333" :background "#E6C384" :weight bold
       :box (:line-width 8 :color "#E6C384")))
  "Face for Evil insert state."
  :group 'punch-line)

(defface punch-line-evil-visual-face
  '((t :foreground "#333333" :background "#D27E99" :weight bold
       :box (:line-width 8 :color "#D27E99")))
  "Face for Evil visual state."
  :group 'punch-line)

(defface punch-line-evil-replace-face
  '((t :foreground "#333333" :background "#FF5D62" :weight bold
       :box (:line-width 8 :color "#FF5D62")))
  "Face for Evil replace state."
  :group 'punch-line)

(defface punch-line-evil-emacs-face
  '((t :foreground "#333333" :background "#B0C4DE" :weight bold
       :box (:line-width 8 :color "#B0C4DE")))
  "Face for Emacs state."
  :group 'punch-line)

;;; Git faces
(defface punch-line-git-edited-face
  '((t :foreground "#F1FA8C"))
  "Face for edited Git files."
  :group 'punch-line)

(defface punch-line-git-added-face
  '((t :foreground "#50FA7B"))
  "Face for added Git files."
  :group 'punch-line)

(defface punch-line-git-removed-face
  '((t :foreground "#FF5555"))
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
  '((t :foreground "#a0a0ae" :weight bold))
  "Face for buffer name."
  :group 'punch-line)

(defface punch-line-major-mode-face
  '((t :foreground "#888899"))
  "Face for major mode."
  :group 'punch-line)

(defface punch-line-position-face
  '((t :foreground "#FFA07A"))
  "Face for buffer position."
  :group 'punch-line)

(defface punch-line-time-face
  '((t :foreground "#888899":weight light))
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

(defface punch-line-eglot-icon-face
  '((t :inherit eglot-mode-line))
  "Standard face for project information."
  :group 'punch-line)

(defface punch-line-separator-face
  '((t :foreground "#54536D" :weight bold :height 0.8))
  "Face for the separator between sections in punch-line."
  :group 'punch-line)


(provide 'punch-line-colors)
;;; punch-line-colors.el ends here
