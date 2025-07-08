;;; cocaine-line-vc.el --- A customized mode-line for Emacs with Evil status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs with Evil status,
;; configurable colors, and the ability to customize displayed information.

;;; Code:

(require 'vc)
(require 'vc-git)
(require 'nerd-icons)

(defvar-local cocaine-git-info-cache nil
  "Cache for Git information.")

(defvar-local cocaine-git-info-cache-time 0
  "Time of last cache update.")

(defcustom cocaine-git-cache-update-interval 5
  "Interval in seconds for updating the Git cache."
  :type 'number
  :group 'cocaine-line)

(defcustom cocaine-show-git-info t
  "If set to t, show Git branch and status."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-line-vc-use-github-icon t
  "If set to t, use the GitHub icon for Git branch."
  :type 'boolean
  :group 'cocaine-line)

;; Use the existing cocaine-line git faces
(defcustom cocaine-git-faces
  '((edited . cocaine-line-git-edited-face)
    (added . cocaine-line-git-added-face)
    (removed . cocaine-line-git-removed-face)
    (conflict . cocaine-line-git-conflict-face)
    (default . cocaine-line-git-face))
  "Faces for different Git states."
  :type '(alist :key-type symbol :value-type face)
  :group 'cocaine-line)

(defun cocaine-vc--rev (file backend)
  "Get the revision for FILE in BACKEND."
  (when-let* ((rev (vc-working-revision file backend)))
    (substring rev 0 (min (length rev) 7))))

(defvar cocaine-git-state-faces
  '((up-to-date . default)
    (edited . edited)
    (added . added)
    (removed . removed)
    (conflict . conflict))
  "Mapping between VC states and face names.")

(defun cocaine-git-icon ()
  "Return the Git icon."
  (if cocaine-line-vc-use-github-icon
      (nerd-icons-octicon "nf-oct-mark_github")
    (nerd-icons-octicon "nf-oct-git_branch")))

(defun cocaine-git-info-create ()
  "Create a cache for Git information."
  (when (and cocaine-show-git-info
             (buffer-file-name)
             (ignore-errors (vc-git-registered (buffer-file-name))))
    (let* ((branch (ignore-errors (vc-git-mode-line-string (buffer-file-name))))
           (state (ignore-errors (vc-state (buffer-file-name))))
           (state-type (alist-get state cocaine-git-state-faces 'default))
           (state-face (alist-get state-type cocaine-git-faces
                                 (alist-get 'default cocaine-git-faces)))
           (status-indicator (if (eq state 'up-to-date) "" "")))
      (when branch
        (concat
         (propertize (cocaine-git-icon) 'face 'mode-line-highlight)
         " "
         (propertize (format " %s%s"
                             (replace-regexp-in-string "^Git[:-]" "" branch)
                             status-indicator)
                     'face state-face))))))

(defun cocaine-git-info ()
  "Show Git branch and status with custom faces."
  (let ((current-time (float-time)))
    (when (or (null cocaine-git-info-cache)
	      (> (- current-time cocaine-git-info-cache-time) cocaine-git-cache-update-interval))
      (setq cocaine-git-info-cache-time current-time)
      (setq cocaine-git-info-cache (cocaine-git-info-create)))
      cocaine-git-info-cache))

(provide 'cocaine-line-vc)
;;; cocaine-line-vc.el ends here 