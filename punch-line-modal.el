;;; punch-line-modal.el --- A customized mode-line for Emacs with Evil status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (evil "1.0.0"))

;;; Commentary:


;;; Code:

(require 'evil)
(require 'evil-mc)
(require 'nerd-icons)

(defcustom punch-line-show-evil-modes t
  "Show Evil modes in the mode-line."
  :type 'boolean
  :group 'punch-line)

;; Update Evil colors to faces
(defcustom punch-evil-faces
  '((normal . punch-line-evil-normal-face)
    (insert . punch-line-evil-insert-face)
    (visual . punch-line-evil-visual-face)
    (replace . punch-line-evil-replace-face))
  "Faces for different Evil states."
  :type '(alist :key-type symbol :value-type face)
  :group 'punch-line)

(defun punch-evil-status-inactive ()
  "Show Evil status with gray face for inactive mode-line."
  (when punch-line-show-evil-modes
    (let* ((evil-state (if (and (bound-and-true-p evil-local-mode)
                                (boundp 'evil-state))
                           evil-state
                         'emacs))
           (state-name (upcase (symbol-name evil-state))))
      (propertize (format " %s " state-name)
                  'face 'punch-line-inactive-face))))

;; Evil status function
(defun punch-evil-status ()
  "Show Evil status with custom face and correct vertical alignment."
  (if punch-line-show-evil-modes
    (let* ((evil-state (if (and (bound-and-true-p evil-local-mode)
                                (boundp 'evil-state)
                                )
                           evil-state
                         ))
           (state-face (or (cdr (assq evil-state punch-evil-faces))
                         'punch-line-evil-emacs-face))
           (state-name (upcase (symbol-name evil-state))))
      (propertize (format " %s " state-name)
                  'face state-face))
    (propertize " " 'face 'punch-line-evil-normal-face)))

(defun punch-evil-mc-info ()
  "Show Evil MC information."
  (let ((cursor-count (evil-mc-get-cursor-count))
        (icon (nerd-icons-octicon "nf-oct-pencil")))
    (if (> cursor-count 1)
        (propertize (format " %s %d " icon cursor-count) 'face '(:inherit punch-line-evil-replace-face))
      "")))

(provide 'punch-line-modal)
;;; punch-line-modal.el ends here
