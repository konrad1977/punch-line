;;; punch-line-vc.el --- A customized mode-line for Emacs with Evil status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs with Evil status,
;; configurable colors, and the ability to customize displayed information.

;;; Code:

(require 'vc)
(require 'vc-git)
(require 'nerd-icons)

(defvar-local punch-git-info-cache nil
  "Cache for Git information.")

(defvar-local punch-git-info-cache-time 0
  "Time of last cache update.")

(defcustom punch-git-cache-update-interval 5
  "Interval in seconds for updating the Git cache."
  :type 'number
  :group 'punch-line)

(defcustom punch-show-git-info t
  "If set to t, show Git branch and status."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-line-vc-use-github-icon t
  "If set to t, use the GitHub icon for Git branch."
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
  (when-let* ((rev (vc-working-revision file backend)))
    (substring rev 0 (min (length rev) 7))))

(defvar punch-git-state-faces
  '((up-to-date . default)
    (edited . edited)
    (added . added)
    (removed . removed)
    (conflict . conflict))
  "Mapping between VC states and face names.")

(defun punch-git-icon ()
  "Return the Git icon."
  (if punch-line-vc-use-github-icon
      (nerd-icons-octicon "nf-oct-mark_github")
    (nerd-icons-octicon "nf-oct-git_branch")))

(defun punch-git-info-create ()
  "Create a cache for Git information."
  (when (and punch-show-git-info
             (buffer-file-name)
             (ignore-errors (vc-git-registered (buffer-file-name))))
    (let* ((branch (ignore-errors (vc-git-mode-line-string (buffer-file-name))))
           (state (ignore-errors (vc-state (buffer-file-name))))
           (state-type (alist-get state punch-git-state-faces 'default))
           (state-face (alist-get state-type punch-git-faces
                                 (alist-get 'default punch-git-faces)))
           (status-indicator (if (eq state 'up-to-date) "" "")))
      (when branch
        (concat
         (propertize (punch-git-icon) 'face 'mode-line-highlight)
         " "
         (propertize (format " %s%s"
                             (replace-regexp-in-string "^Git[:-]" "" branch)
                             status-indicator)
                     'face state-face))))))

(defun punch-git-info ()
  "Show Git branch and status with custom faces."
  (let ((current-time (float-time)))
    (when (or (null punch-git-info-cache)
	      (> (- current-time punch-git-info-cache-time) punch-git-cache-update-interval))
      (setq punch-git-info-cache-time current-time)
      (setq punch-git-info-cache (punch-git-info-create)))
      punch-git-info-cache))

(provide 'punch-line-vc)
;;; punch-line-vc.el ends here
