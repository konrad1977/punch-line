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

(defvar punch-git-repo-cache (make-hash-table :test 'equal)
  "Cache for Git repository information, keyed by directory.")

(defvar punch-git-repo-cache-time (make-hash-table :test 'equal)
  "Cache for Git repository information timestamps.")

(defun punch-git-invalidate-cache ()
  "Invalidate the Git information cache."
  (setq-local punch-git-info-cache nil
              punch-git-info-cache-time 0
              punch-git-file-name nil))

(defun punch-git-invalidate-repo-cache ()
  "Invalidate the Git repository cache."
  (clrhash punch-git-repo-cache)
  (clrhash punch-git-repo-cache-time))

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

(defvar-local punch-git-file-name nil
  "Cache for the file name used in Git info calculation.")

(defun punch-git-repo-registered-p (dir)
  "Check if DIR is a Git repository without spawning a process if possible."
  (let* ((dir (expand-file-name dir))
         (git-dir (expand-file-name ".git" dir)))
    (or (file-exists-p git-dir)
        ;; Try to find .git in parent directories
        (let ((parent (file-name-directory (directory-file-name dir))))
          (and parent
               (not (string= parent dir))
               (punch-git-repo-registered-p parent))))))

(defun punch-git-file-in-repo-p (file-name)
  "Check if FILE-NAME is in a Git repository using cached information when possible."
  (when (and file-name (file-exists-p file-name))
    (let* ((dir (file-name-directory file-name))
           (current-time (float-time))
           (cached-time (gethash dir punch-git-repo-cache-time 0))
           (cached-result (gethash dir punch-git-repo-cache 'not-set)))
      
      ;; If cache is valid, return cached result
      (if (and (not (eq cached-result 'not-set))
               (< (- current-time cached-time) punch-git-cache-update-interval))
          cached-result
        
        ;; Otherwise, check if it's a git repo and cache the result
        (let ((is-git (punch-git-repo-registered-p dir)))
          (puthash dir is-git punch-git-repo-cache)
          (puthash dir current-time punch-git-repo-cache-time)
          is-git)))))

(defun punch-git-info-create ()
  "Create a cache for Git information."
  (let ((file-name (buffer-file-name)))
    (setq punch-git-file-name file-name)
    (when (and punch-show-git-info
               file-name
               (punch-git-file-in-repo-p file-name))
      ;; Only try to get branch info if we know we're in a git repo
      (when-let* ((branch (ignore-errors (vc-git-mode-line-string file-name))))
        (let* ((state (ignore-errors (vc-state file-name)))
               (state-type (alist-get state punch-git-state-faces 'default))
               (state-face (alist-get state-type punch-git-faces
                                     (alist-get 'default punch-git-faces)))
               (status-indicator (if (eq state 'up-to-date) "" "")))
          (concat
           (propertize (punch-git-icon) 'face 'mode-line-highlight)
           " "
           (propertize (format " %s%s"
                               (replace-regexp-in-string "^Git[:-]" "" branch)
                               status-indicator)
                       'face state-face)))))))

(defun punch-git-info ()
  "Show Git branch and status with custom faces."
  (let ((current-time (float-time))
        (file-name (buffer-file-name)))
    ;; Skip git info for non-file buffers entirely
    (if (not file-name)
        ""
      ;; Check if we need to update the cache
      (when (or (null punch-git-info-cache)
                (> (- current-time punch-git-info-cache-time) punch-git-cache-update-interval)
                (not (equal file-name punch-git-file-name)))
        (setq punch-git-info-cache-time current-time)
        (setq punch-git-info-cache (punch-git-info-create)))
      (or punch-git-info-cache ""))))

(provide 'punch-line-vc)
;;; punch-line-vc.el ends here
