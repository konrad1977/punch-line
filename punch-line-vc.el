;;; punch-line-vc.el --- A customized mode-line for Emacs with Evil status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (evil "1.0.0"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs with Evil status,
;; configurable colors, and the ability to customize displayed information.

;;; Code:

(require 'vc)
(require 'vc-git)
(require 'nerd-icons)

(defcustom punch-show-git-info t
  "If set to t, show Git branch and status."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-git-faces
  '((edited . punch-line-git-edited-face)
    (added . punch-line-git-added-face)
    (removed . punch-line-git-removed-face)
    (conflict . punch-line-git-conflict-face)
    (default . punch-line-git-face))
  "Faces for different Git states."
  :type '(alist :key-type symbol :value-type face)
  :group 'punch-line)

(defun punch-vc--rev (file backend)
  "Get the revision for FILE in BACKEND."
  (when-let ((rev (vc-working-revision file backend)))
    (substring rev 0 (min (length rev) 7))))

(defun punch-git-info ()
  "Show Git branch and status with custom faces."
  (when (and punch-show-git-info
             (buffer-file-name)
             (vc-git-registered (buffer-file-name)))
    (let* ((branch (vc-git-mode-line-string (buffer-file-name)))
           (state (vc-state (buffer-file-name)))
           (state-face (alist-get state punch-git-faces
                                  (alist-get 'default punch-git-faces)))
           (status-indicator (if (eq state 'up-to-date) "" "")))
      (when branch
        (propertize (format "%s %s%s"
                            (nerd-icons-octicon "nf-oct-git_branch")
                            (replace-regexp-in-string "^Git[:-]" "" branch)
                            status-indicator)
                    'face state-face)))))


(provide 'punch-line-vc)
;;; punch-line-vc.el ends here
