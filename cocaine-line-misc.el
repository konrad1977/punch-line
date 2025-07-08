;;; cocaine-line-misc.el --- A customized mode-line for Emacs -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs with modal status,
;; configurable colors, and the ability to customize displayed information.
;;; Code:

(require 'nerd-icons)
(require 'cocaine-line-colors)
(require 'project)

(when (featurep 'projectile)
  (require 'projectile))

(when (featurep 'eglot)
  (require 'eglot))

(when (featurep 'flycheck)
  (require 'flycheck))

(defvar-local cocaine-flycheck-cache nil
  "Cache for flycheck information.")

(defvar-local cocaine-flycheck-cache-time nil
  "Time of last cache update.")

(defcustom cocaine-show-processes-info t
  "If set to t, show active processes."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-org-info t
  "If set to t, show org information."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-misc-info nil
  "If set to t, show misc processes."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-copilot-info t
  "If set to t, show copilot icon."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-line-show-time-info t
  "If set to t, show time."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-column-info nil
  "If set to t, show column information."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-project-info t
  "If set to t, show project information."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-lsp-info t
  "If set to t, show LSP information."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-buffer-position nil
  "If set to t, show buffer position."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-flycheck-info t
  "If set to t, show flycheck information."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-flycheck-cache-interval 3
  "Time in seconds to cache flycheck information."
  :type 'integer
  :group 'cocaine-line)

(defun cocaine-flycheck-create-cache ()
  "Create a cache for flycheck information."
  (when (and (bound-and-true-p flycheck-mode)
             cocaine-show-flycheck-info
             (or flycheck-current-errors
                 (eq 'running flycheck-last-status-change)))
    (let* ((count (flycheck-count-errors flycheck-current-errors))
           (info (or (cdr (assq 'info count)) 0))
           (warning (or (cdr (assq 'warning count)) 0))
           (error (or (cdr (assq 'error count)) 0))
           (info-str (when (> info 0)
                      (propertize (format "%s %d" (nerd-icons-codicon "nf-cod-lightbulb") info)
                                'face '(:inherit success))))
           (warning-str (when (> warning 0)
                         (propertize (format "%s %d" (nerd-icons-codicon "nf-cod-warning") warning)
                                   'face '(:inherit warning))))
           (error-str (when (> error 0)
                       (propertize (format "%s %d" (nerd-icons-codicon "nf-cod-error") error)
                                 'face '(:inherit error)))))
      (string-join
       (remove nil
               (list info-str
                     warning-str
                     error-str))
       " "))))

(defun cocaine-flycheck-info ()
  "Return flycheck information, updating the cache if necessary."
  (when cocaine-show-flycheck-info
    (let ((current-time (float-time)))
      (when (or (null cocaine-flycheck-cache)
                (null cocaine-flycheck-cache-time)
                (> (- current-time cocaine-flycheck-cache-time)
                   cocaine-flycheck-cache-interval))
        (setq cocaine-flycheck-cache-time current-time
              cocaine-flycheck-cache (cocaine-flycheck-create-cache))
      ;; Force refresh if cache is empty but should have data
      (when (and (null cocaine-flycheck-cache)
                 (bound-and-true-p flycheck-mode)
                 (or flycheck-current-errors
                     (eq 'running flycheck-last-status-change)))
        (setq cocaine-flycheck-cache (cocaine-flycheck-create-cache))))
      cocaine-flycheck-cache)))

(defun cocaine-process-info ()
  "Show information about active processes."
  (when cocaine-show-processes-info
    (let ((process-info (concat " " (format-mode-line mode-line-process))))
        process-info)))

(defun cocaine-misc-info ()
  "Show information about misc info."
  (when cocaine-show-misc-info
    (let ((misc-info (format-mode-line mode-line-misc-info)))
      (unless (string-blank-p misc-info)
        (string-trim misc-info)))))

(defun cocaine-lsp-info ()
  "Return current LSP (Eglot or lsp-mode) status for the mode line using nerd-icons."
  (when cocaine-show-lsp-info
    (cond
     ((bound-and-true-p eglot--managed-mode)
      (let* ((server (eglot-current-server))
             (icon (propertize (nerd-icons-codicon "nf-cod-pulse")
                               'face 'cocaine-line-eglot-icon-face)))
        (if server
            icon
          "")))
     ((bound-and-true-p lsp-mode)
      (let ((icon (propertize (nerd-icons-codicon "nf-cod-pulse")
                              'face 'cocaine-line-eglot-icon-face)))
        (if (lsp-workspaces)
            icon
          "")))
     (t ""))))

(defun cocaine-project-info ()
  "Show project information."
  (cond
   ((and cocaine-show-project-info cocaine-show-lsp-info)
    (let ((project (cocaine--project-name))
          (lsp (or (cocaine-lsp-info) "")))
      (string-trim (concat project " " lsp))))
   (cocaine-show-project-info
    (cocaine--project-name))
   (cocaine-show-lsp-info
    (or (cocaine-lsp-info) ""))
   (t "")))

(defun cocaine-project-name ()
  "Get the project name if any."
  (or
   (and (fboundp 'project-name)
        (project-current)
        (project-name (project-current)))
   (and (fboundp 'projectile-project-name)
        (projectile-project-name))
   ""))

(defun cocaine--project-name ()
  "Show project information."
  (let ((project (cocaine-project-name)))
    (if (string-empty-p project)
        ""
      (propertize project 'face 'cocaine-line-project-face))))

;; Custom functions for left section
(defun cocaine-buffer-name ()
  "Show buffer name with custom face and icon (if available)."
  (let* ((file-name (buffer-file-name))
         (icon (when file-name
                 (nerd-icons-icon-for-file file-name t)))
         (buffer-name (file-name-sans-extension
                       (substring-no-properties (format-mode-line "%b ")))))
    (if icon
        (concat icon " " (propertize buffer-name 'face 'cocaine-line-buffer-name-face))
      (propertize buffer-name 'face 'cocaine-line-buffer-name-face))))

(defun cocaine-major-mode ()
  "Show major mode with custom face."
  (propertize (substring-no-properties (format-mode-line mode-name))
              'face 'cocaine-line-major-mode-face))

(defun cocaine-line-col ()
  "Show line and column with custom face."
  (when cocaine-show-column-info
    (propertize "[%l:%c]" 'face 'cocaine-line-position-face)))

(defun cocaine-buffer-position ()
  "Show buffer position percentage with custom face."
  (when cocaine-show-buffer-position
    (propertize "%p%" 'face 'cocaine-line-position-face)))

(defun cocaine-copilot-info ()
  "HUD for Copilot."
  (when (bound-and-true-p copilot-mode)
    (propertize " " 'face '(:inherit success))))

(defun cocaine-time ()
  "Show time with custom face."
  (propertize (format-time-string "%H:%M") 'face 'cocaine-line-time-face))

(provide 'cocaine-line-misc)
;;; cocaine-line-misc.el ends here 