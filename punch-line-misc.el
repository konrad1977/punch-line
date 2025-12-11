;;; punch-line-misc.el --- A customized mode-line for Emacs -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs with modal status,
;; configurable colors, and the ability to customize displayed information.
;;; Code:

(require 'eglot)
(require 'nerd-icons)
(require 'project)
(require 'punch-line-colors)

(when (featurep 'projectile)
  (require 'projectile))

(when (featurep 'flycheck)
  (require 'flycheck))

(when (featurep 'flymake)
  (require 'flymake))

;; Declare functions to avoid byte-compilation warnings
(declare-function flycheck-count-errors "flycheck")
(declare-function flymake-diagnostics "flymake")
(declare-function flymake-diagnostic-type "flymake")

(defvar-local punch-diagnostics-cache nil
  "Cache for diagnostics information.")

(defvar-local punch-diagnostics-cache-timer nil
  "Timer for clearing diagnostics cache.")

(defvar-local punch-diagnostics-cache-time nil
  "Timestamp of last cache update.")

(defgroup punch-line nil
  "Customization group for punch-line."
  :group 'convenience)

(defcustom punch-show-buffer-name t
  "If set to t, show buffer name in the mode line."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-major-mode t
  "If set to t, show major mode in the mode line."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-processes-info t
  "If set to t, show active processes."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-org-info t
  "If set to t, show org information."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-misc-info nil
  "If set to t, show misc processes."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-copilot-info t
  "If set to t, show copilot icon."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-line-show-time-info t
  "If set to t, show time."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-column-info nil
  "If set to t, show column information."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-project-info t
  "If set to t, show project information."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-lsp-info t
  "If set to t, show LSP information."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-buffer-position nil
  "If set to t, show buffer position."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-diagnostics-info t
  "If set to t, show diagnostics information."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-diagnostics-checker 'auto
  "Diagnostic checker preference for punch-line.
When set to 'auto, automatically detect available checker.
When set to 'flycheck, prefer flycheck if available.
When set to 'flymake, prefer flymake if available."
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "Flycheck" flycheck)
                 (const :tag "Flymake" flymake))
  :group 'punch-line)

(defcustom punch-diagnostics-cache-interval 3
  "Time in seconds to cache diagnostics information."
  :type 'integer
  :group 'punch-line)

(defun punch-diagnostics-create-cache ()
  "Create a cache for diagnostics information based on active checker."
  (when punch-show-diagnostics-info
    (let ((info 0) (warning 0) (error 0))
      ;; Get counts from active checker
      (cond
       ;; Flycheck
       ((and (or (eq punch-diagnostics-checker 'flycheck)
                 (eq punch-diagnostics-checker 'auto))
             (bound-and-true-p flycheck-mode)
             (or flycheck-current-errors
                 (eq 'running flycheck-last-status-change)))
        (let ((count (flycheck-count-errors flycheck-current-errors)))
          (setq info (or (cdr (assq 'info count)) 0)
                warning (or (cdr (assq 'warning count)) 0)
                error (or (cdr (assq 'error count)) 0))))
       ;; Flymake
       ((and (or (eq punch-diagnostics-checker 'flymake)
                 (eq punch-diagnostics-checker 'auto))
             (bound-and-true-p flymake-mode))
        (dolist (diag (flymake-diagnostics))
          (pcase (flymake-diagnostic-type diag)
            (:note (setq info (1+ info)))
            (:warning (setq warning (1+ warning)))
            (:error (setq error (1+ error)))))))
      ;; Format and return
      (when (> (+ info warning error) 0)
        (string-join
         (remove nil
                 (list (when (> info 0)
                         (propertize (format "%s %d" (nerd-icons-codicon "nf-cod-lightbulb") info)
                                     'face '(:inherit success)))
                       (when (> warning 0)
                         (propertize (format "%s %d" (nerd-icons-codicon "nf-cod-warning") warning)
                                     'face '(:inherit warning)))
                       (when (> error 0)
                         (propertize (format "%s %d" (nerd-icons-codicon "nf-cod-error") error)
                                     'face '(:inherit error)))))
         " ")))))

(defun punch-diagnostics-info ()
  "Return diagnostics information, updating the cache if necessary."
  (when punch-show-diagnostics-info
    (let ((current-time (float-time)))
      (when (or (null punch-diagnostics-cache)
                (null punch-diagnostics-cache-time)
                (> (- current-time punch-diagnostics-cache-time)
                   punch-diagnostics-cache-interval))
        (setq punch-diagnostics-cache-time current-time
              punch-diagnostics-cache (punch-diagnostics-create-cache)))
      ;; Force refresh if cache is empty but should have data
      (when (and (null punch-diagnostics-cache)
                 (or (bound-and-true-p flycheck-mode)
                     (bound-and-true-p flymake-mode)))
        (setq punch-diagnostics-cache (punch-diagnostics-create-cache)))
      punch-diagnostics-cache)))

;; Deprecated alias for backward compatibility
(defalias 'punch-flycheck-info 'punch-diagnostics-info
  "Deprecated alias for `punch-diagnostics-info'.")


(defun punch-process-info ()
  "Show information about active processes."
  (when punch-show-processes-info
    (let ((process-info (concat " " (format-mode-line mode-line-process))))
        process-info)))

(defun punch-misc-info ()
  "Show information about misc info."
  (when punch-show-misc-info
    (let ((misc-info (format-mode-line mode-line-misc-info)))
      (unless (string-blank-p misc-info)
        (string-trim misc-info)))))

(defun punch-lsp-info ()
  "Return current LSP (Eglot or lsp-mode) status for the mode line using nerd-icons."
  (when punch-show-lsp-info
    (cond
     ((bound-and-true-p eglot--managed-mode)
      (let* ((server (eglot-current-server))
             (icon (propertize (nerd-icons-codicon "nf-cod-pulse")
                               'face 'punch-line-lsp-icon-face)))
        (if server
            icon
          "")))
     ((bound-and-true-p lsp-mode)
      (let ((icon (propertize (nerd-icons-codicon "nf-cod-pulse")
                              'face 'punch-line-lsp-icon-face)))
        (if (lsp-workspaces)
            icon
          "")))
     (t ""))))

(defun punch-project-info ()
  "Show project information."
  (cond
   ((and punch-show-project-info punch-show-lsp-info)
    (let ((project (punch--project-name))
          (lsp (or (punch-lsp-info) "")))
      (string-trim (concat project " " lsp))))
   (punch-show-project-info
    (punch--project-name))
   (punch-show-lsp-info
    (or (punch-lsp-info) ""))
   (t "")))

(defun punch-project-name ()
  "Get the project name if any."
  (or
   (and (fboundp 'project-name)
        (project-current)
        (project-name (project-current)))
   (and (fboundp 'projectile-project-name)
        (projectile-project-name))
   ""))

(defun punch--project-name ()
  "Show project information."
  (let ((project (punch-project-name)))
    (if (string-empty-p project)
        ""
      (propertize project 'face 'punch-line-project-face))))

;; Custom functions for left section
(defun punch-buffer-name ()
  "Show buffer name with custom face and icon (if available)."
  (let* ((file-name (buffer-file-name))
         (icon (when file-name
                 (nerd-icons-icon-for-file file-name t)))
         (buffer-name (file-name-sans-extension
                       (substring-no-properties (format-mode-line "%b ")))))
    (if icon
        (concat icon " " (propertize buffer-name 'face 'punch-line-buffer-name-face))
      (propertize buffer-name 'face 'punch-line-buffer-name-face))))

(defun punch-major-mode ()
  "Show major mode with custom face."
  (propertize (substring-no-properties (format-mode-line mode-name))
              'face 'punch-line-major-mode-face))

(defun punch-line-col ()
  "Show line and column with custom face."
  (when punch-show-column-info
    (propertize "[%l:%c]" 'face 'punch-line-position-face)))

(defun punch-buffer-position ()
  "Show buffer position percentage with custom face."
  (when punch-show-buffer-position
    (propertize "%p%" 'face 'punch-line-position-face)))

(defun punch-copilot-info ()
  "HUD for Copilot."
  (when (and punch-show-copilot-info (bound-and-true-p copilot-mode))
    (propertize " " 'face '(:inherit success))))

(provide 'punch-line-misc)
;;; punch-line-misc.el ends here
